## Copyright (C) 1995-2012 Friedrich Leisch
## Copyright (C) 2010 Alois Schloegl
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {[Pxx, @var{w}] =} periodogram (@var{x})
## For a data matrix @var{x} from a sample of size @var{n}, return the
## periodogram.  The angular frequency is returned in @var{w}.
##
## [Pxx,w] = periodogram (@var{x}).
##
## [Pxx,w] = periodogram (@var{x},win).
##
## [Pxx,w] = periodogram (@var{x},win,nfft).
##
## [Pxx,f] = periodogram (@var{x},win,nfft,Fs).
##
## [Pxx,f] = periodogram (@var{x},win,nfft,Fs,"range").
##
## @itemize
## @item x: data; if real-valued a one-sided spectrum is estimated,
## if complex-valued or range indicates "@nospell{twosided}", the full
## spectrum is estimated.
##
## @item win: weight data with window, x.*win is used for further computation,
## if window is empty, a rectangular window is used.
##
## @item nfft: number of frequency bins, default max(256, 2.^ceil(log2(length(x)))).
##
## @item Fs: sampling rate, default 1.
##
## @item range: "@nospell{onesided}" computes spectrum from [0..nfft/2+1].
## "@nospell{twosided}" computes spectrum from [0..nfft-1].  These strings
## can appear at any position in the list input arguments after window.
##
## @item Pxx: one-, or two-sided power spectrum.
##
## @item w: angular frequency [0..2*pi) (two-sided) or [0..pi] one-sided.
##
## @item f: frequency [0..Fs) (two-sided) or [0..Fs/2] one-sided.
## @end itemize
## @end deftypefn

## Author: FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description: Compute the periodogram

function [pxx, f] = periodogram (x, varargin)

  ## check input arguments

  if (nargin < 1 || nargin > 5)
    print_usage ();
  endif

  nfft = []; fs = []; range = []; window = [];
  j = 1;
  for k = 1:length (varargin)
    if (ischar (varargin{k}))
      range = varargin{k};
    else
      switch (j)
      case 1
        window = varargin{k};
      case 2
        nfft   = varargin{k};
      case 3
        fs     = varargin{k};
      case 4
        range  = varargin{k};
      endswitch
      j++;
    endif
  endfor

  [r, c] = size (x);
  if (r == 1)
    r = c;
  endif

  if (ischar (window))
    range = window;
    window = [];
  endif;
  if (ischar (nfft))
    range = nfft;
    nfft = [];
  endif;
  if (ischar (fs))
    range = fs;
    fs = [];
  endif;

  if (!  isempty (window))
    if (all (size (x) == size (window)))
      x .*= window;
    elseif (size (x, 1) == size (window, 1) && size (window, 2) == 1)
      x .*= window (:,ones (1,c));
    endif;
  endif

  if (numel (nfft)>1)
    error ("nfft must be scalar");
  endif
  if (isempty (nfft))
    nfft = max (256, 2.^ceil (log2 (r)));
  endif

  if (strcmp (range, "onesided"))
    range = 1;
  elseif strcmp (range, "twosided")
    range = 2;
  else
    range = 2-isreal (x);
  endif

  ## compute periodogram

  if (r>nfft)
    Pxx = 0;
    rr = rem (length (x), nfft);
    if (rr)
      x = [x(:); (zeros (nfft-rr, 1))];
    endif
    x = sum (reshape (x, nfft, []), 2);
  endif

  if (isempty (window))
    n = r;
  else
    n = sumsq (window);
  end;
  Pxx = (abs (fft (x, nfft))) .^ 2 / n ;

  if (nargin<4)
    Pxx /= 2*pi;
  elseif (! isempty (fs))
    Pxx /= fs;
  endif

  ## generate output arguments

  if (range == 1)  # onesided
    Pxx = Pxx(1:nfft/2+1) + [0; Pxx(end:-1:(nfft/2+2)); 0];
  endif

  if (nargout != 1)
    if (range == 1)
      f = (0:nfft/2)'/nfft;
    elseif (range == 2)
      f = (0:nfft-1)'/nfft;
    endif
    if (nargin<4)
      f *= 2*pi; # generate w=2*pi*f
    elseif (! isempty (fs))
      f *= fs;
    endif
  endif

  if (nargout == 0)
    if (nargin<4)
      plot (f/(2*pi), 10*log10 (Pxx));
      xlabel ("normalized frequency [x pi rad]");
      ylabel ("Power density [dB/rad/sample]");
    else
      plot (f, 10*log10 (Pxx));
      xlabel ("frequency [Hz]");
      ylabel ("Power density [dB/Hz]");
    endif
    grid on;
    title ("Periodogram Power Spectral Density Estimate");
  else
    pxx = Pxx;
  endif

endfunction
