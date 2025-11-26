########################################################################
##
## Copyright (C) 2025 The Octave Project Developers
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
## @deftypefn  {} {@var{__pkg__} =} get_validated_pkg_list ()
## @deftypefnx {} {@var{__pkg__} =} get_validated_pkg_list (@var{force_refresh})
## @deftypefnx {} {@var{__pkg__} =} get_validated_pkg_list (@var{force_refresh}, @var{verbose})
## Download or load cached list of current packages and validate that it fits
## expected patterns.
##
## If @var{force_refresh} is true, always download fresh data from the server
## and update the cache.
##
## If @var{verbose} is true, print diagnostic messages about cache operations.
##
## Return @code{__pkg__} struct used by other functions.
## @end deftypefn

function retval = get_validated_pkg_list (force_refresh = false, verbose = false)

  ## The __pkg__ struct is what we return with all the package information.
  ## We make it persistent to avoid querying the server each time.
  persistent __pkg__;

  if (! isempty (__pkg__) && ! force_refresh)
    ## This function has been called already and __pkg__ exists.
    ## No need to query the server again unless refresh is forced.
    retval = __pkg__;
    return;
  endif

  ## Define cache directory location
  cache_dir = fullfile (user_config_dir (), "octave");

  ## Cache files use timestamped names: packages_yyyymmddHHMM.json
  ## This makes age visible and provides natural history/rollback
  cache_pattern = "packages_*.json";

  ## Maximum age of cache in days (7 days)
  max_cache_age = 7;

  ## Maximum number of old cache files to keep (for history/rollback)
  max_cache_files = 3;

  ## Find most recent cache file
  cache_file = "";
  cache_age_days = Inf;

  if (exist (cache_dir, "dir"))
    cache_files = dir (fullfile (cache_dir, cache_pattern));
    if (! isempty (cache_files))
      if (verbose)
        printf ("pkg: searching for cached package database in %s\n", cache_dir);
      endif

      ## Find most recent cache by parsing timestamps from filenames
      most_recent_time = 0;
      for i = 1:numel (cache_files)
        fname = cache_files(i).name;
        ## Extract timestamp: packages_yyyymmddHHMM.json
        if (length (fname) == 26 && strcmp (fname(1:9), "packages_") &&
            strcmp (fname(22:26), ".json"))
          timestamp_str = fname(10:21);

          try
            ## Use datenum with format string
            file_time = datenum (timestamp_str, "yyyymmddHHMM");

            if (file_time > most_recent_time)
              most_recent_time = file_time;
              cache_file = fullfile (cache_dir, fname);
            endif
          catch
            ## Skip malformed timestamp
            if (verbose)
              warning ("pkg: skipping cache file with malformed timestamp: %s", fname);
            endif
          end_try_catch
        endif
      endfor

      ## Calculate age if we found a cache file
      if (! isempty (cache_file))
        cache_age_days = (now () - most_recent_time);
        if (verbose)
          printf ("pkg: found cached database from %s (%.1f days old)\n",
                  datestr (most_recent_time, "yyyy-mm-dd HH:MM"),
                  cache_age_days);
        endif
      endif
    endif
  endif

  use_cache = false;
  need_update = false;

  if (! force_refresh && ! isempty (cache_file))
    ## Cache file exists, check its age
    if (cache_age_days < max_cache_age)
      ## Cache is fresh enough, use it
      use_cache = true;
      if (verbose)
        printf ("pkg: cache is fresh (< %d days old), using cached data\n",
                max_cache_age);
      endif
    else
      ## Cache is too old
      need_update = true;
      if (verbose)
        printf ("pkg: cache is stale (>= %d days old), update recommended\n",
                max_cache_age);
      endif
    endif
  endif

  ## Try to use cache if available and fresh
  if (use_cache)
    if (verbose)
      printf ("pkg: reading package database from cache file...\n");
    endif
    try
      fid = fopen (cache_file, "rt");
      if (fid < 0)
        error ("pkg: could not open cache file for reading");
      endif
      unwind_protect
        list = fread (fid, Inf, "*char")';
      unwind_protect_cleanup
        fclose (fid);
      end_unwind_protect

      __pkg__ = jsondecode (list, "makeValidName", false);

      if (! isstruct (__pkg__))
        warning ("pkg: cached data has unexpected format, will try to refresh");
        use_cache = false;
        need_update = true;
      else
        if (verbose)
          printf ("pkg: successfully loaded %d packages from cache\n",
                  numel (fieldnames (__pkg__)));
        endif
        retval = __pkg__;
        return;
      endif
    catch
      warning ("pkg: error reading cache file, will try to refresh: %s", ...
               lasterr ());
      use_cache = false;
      need_update = true;
    end_try_catch
  endif

  ## If cache is old, ask user whether to update (unless already forced)
  if (need_update && ! force_refresh)
    ## Interactive prompt - ask user
    response = input (sprintf ([ ...
      "The package database cache is more than %d days old.\n", ...
      "Would you like to update it now? (Y/n): "], max_cache_age), "s");

    if (isempty (response) || any (strcmpi (response, {"y", "yes"})))
      force_refresh = true;
    else
      ## User declined to update
      if (! isempty (cache_file))
        ## Use old cache anyway
        warning ("pkg: using outdated cache, package information may be stale");
        if (verbose)
          printf ("pkg: reading from outdated cache file...\n");
        endif
        try
          fid = fopen (cache_file, "rt");
          if (fid >= 0)
            unwind_protect
              list = fread (fid, Inf, "*char")';
            unwind_protect_cleanup
              fclose (fid);
            end_unwind_protect

            __pkg__ = jsondecode (list, "makeValidName", false);

            if (isstruct (__pkg__))
              if (verbose)
                printf ("pkg: loaded %d packages from outdated cache\n",
                        numel (fieldnames (__pkg__)));
              endif
              retval = __pkg__;
              return;
            endif
          endif
        catch
          ## Fall through to online fetch
        end_try_catch
      endif
    endif
  endif

  ## Download fresh data from server
  downloaded_fresh_data = false;

  if (verbose || force_refresh)
    printf ("pkg: downloading latest package database from packages.octave.org...\n");
    if (! verbose)
      printf ("pkg: this may take a moment, please wait...\n");
    endif
  endif

  ## Try primary URL first, then backup
  primary_url = "https://packages.octave.org/packages.json";
  backup_url = "https://gnu-octave.github.io/packages/packages.json";

  [list, succ] = urlread (primary_url);

  if (! succ)
    ## Primary failed, try backup
    if (verbose)
      printf ("pkg: primary repository unavailable, trying backup from gnu-octave.github.io...\n");
    endif
    [list, succ] = urlread (backup_url);
  endif

  if (! succ)
    ## Could not download - try to use cache as fallback
    if (verbose)
      warning ("pkg: download failed, attempting to use cached data");
    endif
    if (! isempty (cache_file))
      warning ("pkg: could not download package list, using cached version");
      try
        fid = fopen (cache_file, "rt");
        if (fid >= 0)
          unwind_protect
            list = fread (fid, Inf, "*char")';
          unwind_protect_cleanup
            fclose (fid);
          end_unwind_protect

          __pkg__ = jsondecode (list, "makeValidName", false);

          if (isstruct (__pkg__))
            if (verbose)
              printf ("pkg: successfully fell back to cached data (%d packages)\n",
                      numel (fieldnames (__pkg__)));
            endif
            retval = __pkg__;
            return;
          endif
        endif
      catch
        error ("pkg: could not read URL and cache is unavailable, please verify internet connection");
      end_try_catch
    else
      error ("pkg: could not read URL and no cache available, please verify internet connection");
    endif
  else
    ## Download succeeded - capture timestamp
    ## This reflects actual data freshness, not when we started trying
    download_time = now ();
    downloaded_fresh_data = true;
  endif

  if (verbose)
    printf ("pkg: download complete, parsing package database...\n");
  endif

  __pkg__ = jsondecode (list, "makeValidName", false);

  ## A sanity check before the calling location uses this.
  if (! isstruct (__pkg__))
    error ("pkg: server returned data of unknown format");
  endif

  if (verbose)
    printf ("pkg: successfully parsed %d packages\n", numel (fieldnames (__pkg__)));
  endif

  ## Save to cache with timestamp in filename (only if we downloaded fresh data)
  if (downloaded_fresh_data)
    try
      ## Create cache directory if it doesn't exist
      if (! exist (cache_dir, "dir"))
        if (verbose)
          printf ("pkg: creating cache directory: %s\n", cache_dir);
        endif
        [success, msg] = mkdir (cache_dir);
        if (! success)
          warning ("pkg: could not create cache directory: %s", msg);
        endif
      endif

    ## Generate timestamped filename using datestr
    ## Format: packages_yyyymmddHHMM.json
    ## Use download_time, not current time, so timestamp reflects data age
    timestamp_str = datestr (download_time, "yyyymmddHHMM");
    new_cache_file = fullfile (cache_dir, ["packages_" timestamp_str ".json"]);

    if (verbose)
      printf ("pkg: saving package database to cache: %s\n", new_cache_file);
    endif

    ## Write new cache
    fid = fopen (new_cache_file, "wt");
    if (fid >= 0)
      unwind_protect
        fwrite (fid, list);
      unwind_protect_cleanup
        fclose (fid);
      end_unwind_protect

      if (verbose)
        printf ("pkg: cache file saved successfully\n");
      endif

      ## Clean up old cache files (keep only max_cache_files most recent)
      cache_files = dir (fullfile (cache_dir, cache_pattern));
      if (numel (cache_files) > max_cache_files)
        if (verbose)
          printf ("pkg: cleaning up old cache files (keeping %d most recent)...\n",
                  max_cache_files);
        endif
        ## Sort by modification time
        [~, idx] = sort ([cache_files.datenum], "descend");
        ## Remove oldest files
        for i = (max_cache_files + 1):numel (idx)
          old_file = fullfile (cache_dir, cache_files(idx(i)).name);
          try
            unlink (old_file);
            if (verbose)
              printf ("pkg: deleted old cache file: %s\n", cache_files(idx(i)).name);
            endif
          catch
            warning ("pkg: could not delete old cache file: %s", old_file);
          end_try_catch
        endfor
      endif
      else
        warning ("pkg: could not write to cache file");
      endif
    catch
      warning ("pkg: error updating cache: %s", lasterr ());
    end_try_catch
  endif  ## if (downloaded_fresh_data)

  retval = __pkg__;

endfunction
