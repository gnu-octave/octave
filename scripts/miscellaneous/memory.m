########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} memory ()
## @deftypefnx {} {[@var{userdata}, @var{systemdata}] =} memory ()
## Display or return information about the memory usage of Octave.
##
## If the function is called without output arguments, a table with an overview
## of the current memory consumption is displayed.
##
## The output argument @var{userdata} is a structure with the following fields
## containing data for the Octave process:
##
## @table @code
## @item MaxPossibleArrayBytes
## Maximum size for an array to be allocated.  Be aware that this includes
## @emph{all} physical memory and swap space.  Allocating that amount of memory
## might result in system instability, data corruption, and/or file system
## corruption.  Note that depending on the platform (32-bit systems), the
## largest contiguous memory block might further limit the maximum possible
## allocatable array.  This check is not currently implemented.
##
## @item MemAvailableAllArrays
## The total size of available memory in bytes.
##
## @item ram_available_all_arrays
## The maximum size for an array that can be allocated in physical memory
## (excluding swap space).  Note that depending on the platform (32-bit
## systems), the largest contiguous memory block might further limit the
## maximum possible allocatable array.  This check is not currently
## implemented.
##
## @item  MemUsedMATLAB
## @itemx mem_used_octave
## The memory (including swap space) currently used by Octave in bytes.
##
## @item ram_used_octave
## The physical memory (excluding swap space) currently used by Octave in
## bytes.
##
## @end table
##
## The output argument @var{systemdata} is a nested structure with the
## following fields containing information about the system's memory:
##
## @table @code
## @item PhysicalMemory.Available
## The currently available physical memory in bytes.
##
## @item PhysicalMemory.Total
## The total physical memory in bytes.
##
## @item SystemMemory.Available
## The currently available memory (including swap space) in bytes.
##
## @item SystemMemory.Total
## The total memory (including swap space) in bytes.
##
## @item VirtualAddressSpace.Available
## The currently available virtual address space in bytes.
##
## @item VirtualAddressSpace.Total
## The total virtual address space in bytes.
##
## @end table
##
## Example #1 : print formatted table of memory usage
##
## @example
## @group
## memory ()
## @result{}
## System    RAM: 3934008 KiB,  swap: 4087804 KiB
## Octave    RAM:  170596 KiB,  virt: 1347944 KiB
## Free      RAM: 1954940 KiB,  swap: 4087804 KiB
## Available RAM: 2451948 KiB, total: 6042744 KiB
## @end group
## @end example
##
## Example #2 : return structs with memory usage information
##
## @example
## [userdata, systemdata] = memory ()
## @result{}
##  userdata =
##
##    scalar structure containing the fields:
##
##      MaxPossibleArrayBytes = 6.1622e+09
##      MemAvailableAllArrays = 6.1622e+09
##      ram_available_all_arrays = 2.4883e+09
##      MemUsedMATLAB = 1.3825e+09
##      mem_used_octave = 1.3825e+09
##      ram_used_octave = 1.7824e+08
##
##  systemdata =
##
##    scalar structure containing the fields:
##
##      PhysicalMemory =
##
##        scalar structure containing the fields:
##
##          Available = 2.4954e+09
##          Total = 4.0284e+09
##
##      SystemMemory =
##
##        scalar structure containing the fields:
##
##          Available = 6.6813e+09
##          Total = 8.2143e+09
##
##      VirtualAddressSpace =
##
##        scalar structure containing the fields:
##
##          Available = 2.8147e+14
##          Total = 2.8147e+14
## @end example
##
## Programming Note: This function is implemented for Linux and Windows only.
##
## @seealso{computer, getpid, getrusage, nproc, uname}
## @end deftypefn

function [userdata, systemdata] = memory ()

  if ((! isunix () || ismac ()) && ! ispc ())
    if (nargout > 0)
      error ("memory: function not yet implemented for this architecture");
    else
      warning ("memory: function not yet implemented for this architecture");
    endif
    return;
  endif

  kiB = 1024;
  [architecture, bits] = computer ();

  if (isunix () && ! ismac ())
    ## Read values from pseudofiles
    [status, meminfo] = lmemory ();

    ## FIXME: Query the actual size of the user address space,
    ##        e.g., with getrlimit (RLIMIT_AS, rlp)
    if (log2 (bits) > 32)
      ## 64-bit platform
      address_space = 2^48;  # 256 TiB
    else
      ## 32-bit platform
      address_space = 3 * 2^30;  # 3 GiB
    endif

    total_ram = meminfo.MemTotal * kiB;
    total_swap = meminfo.SwapTotal * kiB;
    free_ram = meminfo.MemFree * kiB;
    if (isfield (meminfo, "MemAvailable"))
      available_ram = meminfo.MemAvailable * kiB;
    else
      ## On kernels from before 2014 MemAvailable is not present.
      ## This is a rough estimate that can be used instead.
      available_ram = (meminfo.MemFree + meminfo.Cached) * kiB;
    endif
    free_swap = meminfo.SwapFree * kiB;
    used_ram = status.VmRSS * kiB;
    used_virtual = status.VmSize * kiB;
    avail_virtual = address_space - used_virtual;

  elseif (ispc ())
    [proc, sys] = __wmemory__ ();

    total_ram = sys.TotalPhys;
    total_swap = sys.TotalPageFile;
    available_ram = sys.AvailPhys;
    free_swap = sys.AvailPageFile;
    used_ram = proc.WorkingSetSize;
    used_virtual = proc.WorkingSetSize + proc.PagefileUsage;
    avail_virtual = sys.AvailVirtual;
    address_space = sys.TotalVirtual;

  endif

  available = min (available_ram + free_swap, avail_virtual);
  ram_available = min (available_ram, avail_virtual);

  ## FIXME: On 32-bit systems, the largest possible array is limited by the
  ##        largest contiguous block in memory.
  user.MaxPossibleArrayBytes = available;
  user.MemAvailableAllArrays = available;
  user.ram_available_all_arrays = ram_available;
  user.MemUsedMATLAB = used_virtual;  # For compatibility
  user.mem_used_octave = used_virtual;
  user.ram_used_octave = used_ram;

  syst.PhysicalMemory.Available = available_ram;
  syst.PhysicalMemory.Total = total_ram;
  syst.SystemMemory.Available = available_ram + free_swap;
  syst.SystemMemory.Total = total_ram + total_swap;
  syst.VirtualAddressSpace.Available = avail_virtual;
  syst.VirtualAddressSpace.Total = address_space;

  if (nargout)
    userdata = user;
    systemdata = syst;
  else
    unitsize = kiB;
    unitname = 'kiB';
    disp (sprintf ("Octave is running on %s", architecture))
    disp (sprintf ("System    RAM: %9.0f %s,  swap: %9.0f %s",
                   round (syst.PhysicalMemory.Total / unitsize), unitname,
                   round (total_swap / unitsize), unitname ))
    disp (sprintf ("Octave    RAM: %9.0f %s,  virt: %9.0f %s",
                   round (user.ram_used_octave / unitsize), unitname,
                   round (user.mem_used_octave / unitsize), unitname))
    if (isunix ())
      ## The concept of free vs. available RAM doesn't seem to exist on Windows
      disp (sprintf ("Free      RAM: %9.0f %s,  swap: %9.0f %s",
                     round (free_ram / unitsize), unitname,
                     round (free_swap / unitsize), unitname))
    endif
    disp (sprintf ("Available RAM: %9.0f %s, total: %9.0f %s",
                   round (user.ram_available_all_arrays / unitsize), unitname,
                   round (user.MemAvailableAllArrays / unitsize), unitname))
  endif

endfunction

function [status, meminfo] = lmemory ()

  ## Read pseudo files to gather memory information on Linux

  ## Read the proc/self/status pseudofile.
  ## See https://linuxwiki.de/proc/pid#A.2Fproc.2Fpid.2Fstatus.
  ## It contains a variable number of lines with name-value pairs.

  f = fopen ("/proc/self/status");
  buffer = textscan (f, "%s %s", "delimiter", ':\n');
  fclose (f);
  for i = 1:rows (buffer{1})
    status.(buffer{1}{i}) = textscan (buffer{2}{i}){1};
  endfor

  ## Read the /proc/meminfo pseudofile
  ## see https://linuxwiki.de/proc/meminfo
  ## It contains a variable number of lines with name-value pairs.

  f = fopen ("/proc/meminfo");
  buffer = textscan (f, "%s %s", "delimiter", ':\n');
  fclose (f);
  for i = 1:rows (buffer{1})
    meminfo.(buffer{1}{i}) = textscan (buffer{2}{i}){1};
  endfor

endfunction


%!testif ; (isunix () && ! ismac ()) || ispc ()
%! [user, syst] = memory ();
%! assert (user.mem_used_octave > 0);
%! assert (user.ram_used_octave <= user.mem_used_octave);
%! assert (user.mem_used_octave < syst.SystemMemory.Total);
%! assert (user.MemAvailableAllArrays <= syst.SystemMemory.Available);

%!testif ; (! isunix () || ismac ()) && ! ispc ()
%! fail ("[user] = memory ()",
%!       "function not yet implemented for this architecture");
%! fail ("memory ()", "warning",
%!       "function not yet implemented for this architecture");
