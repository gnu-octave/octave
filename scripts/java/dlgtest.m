%
% Test the dlg... functions of the Octave-Java bridge
%
% Once the Java bridge works OK this function should be dropped from core octave
%
% Author: Martin Hepperle
% Version August 2010
% Adapted for core Octave Philip Nienhuis 2012
%
function dlgtest

  answer = 1;
  while (answer > 0 )

    disp('');
    disp('0 ... STOP');
    disp('1 ... listdlg tests');
    disp('2 ... errordlg tests');
    disp('3 ... warndlg tests');
    disp('4 ... helpdlg tests');
    disp('5 ... inputdlg tests');
    disp('6 ... TeX code tests');
    
    answer = str2num(input ('Run which test?   [0] > ','s'));

    disp('');
    
    switch answer
      case 1
        test_listdlg();
      case 2
        test_errordlg();
      case 3
        test_warndlg();
      case 4
        test_helpdlg();
      case 5
        test_inputdlg();
      case 6
        test_TeXCodes();
    end
  end

   %   d = javaObject('javax.swing.JDialog');
   %   cp = d.getContentPane;
   %   b = javaObject('javax.swing.JButton','OK');
   %   cp.add(b);
   %   d.pack;
   %   d.setVisible(true);


   page_screen_output(1);

end

function test_listdlg

   %-----------------------------------------------
   disp('- test listdlg with selectionmode single. No caption, no prompt.');
   itemlist = {'An item \\alpha', 'another', 'yet another'};
   s = listdlg ( 'ListString',itemlist, 'SelectionMode','Single' );
   imax = length(s);
   for i=1:1:imax
      disp(['Selected: ',num2str(i),': ', itemlist{s(i)}]);
   end

   %-----------------------------------------------
   disp('- test listdlg with selectionmode and preselection. Has caption and two lines prompt.');
   s = listdlg ( 'ListString',itemlist, ...
                 'SelectionMode','Multiple', ...
                 'Name','Selection Dialog', ...
                 'InitialValue',[1,2,3,4],
                 'PromptString',{'Select an item...', '...or multiple items'} );
   imax = length(s);
   for i=1:1:imax
      disp(['Selected: ',num2str(i),': ', itemlist{s(i)}]);
   end

end

function test_errordlg
   %-----------------------------------------------
   disp('- test errordlg with prompt only.');
   errordlg('Oops, an expected error occured');
   %-----------------------------------------------
   disp('- test errordlg with prompt and caption.');
   errordlg('Oops another error','This is a very long and informative caption');
end

function test_warndlg
   %-----------------------------------------------
   disp('- test warndlg with prompt only.');
   warndlg('Oh, a warning occured');
   %-----------------------------------------------
   disp('- test warndlg with prompt and caption.');
   warndlg('Oh, No...','This is the last Warning');
end

function test_helpdlg
   %-----------------------------------------------
   disp('- test helpdlg with a help message only.');
   helpdlg("Below, you should see 3 lines:\nline #1\nline #2, and\nline #3.");
   %-----------------------------------------------
   disp('- test helpdlg with help message and caption.');
   helpdlg('You should see a single line.','A help dialog');
end

function test_inputdlg
   %-----------------------------------------------
   disp('- test inputdlg with prompt and caption only.');
   prompt = {'Width','Height','Depth'};
   dims = inputdlg ( prompt, 'Enter Box Dimensions' );
   if isempty(dims)
      helpdlg('Canceled by user', 'Information');
   else
      volume  = str2num(dims{1}) * str2num(dims{2}) * str2num(dims{3});
      surface = 2 * (str2num(dims{1}) * str2num(dims{2}) + ...
                     str2num(dims{2}) * str2num(dims{3}) + ...
                     str2num(dims{1}) * str2num(dims{3}));
      helpdlg(sprintf('Results:\nVolume = %.3f\nSurface = %.3f', volume, surface), 'Box Dimensions');
   end

   %-----------------------------------------------
   disp('- test inputdlg with prescribed scalar (2 lines per text field) and defaults.');
   prompt = {'Width','Height','Depth'};
   default = {'1.1','2.2','3.3'};
   rc = 2;
   dims = inputdlg ( prompt, 'Enter Box Dimensions',rc,default );
   if isempty(dims)
      helpdlg('Canceled by user', 'Information');
   else
      volume  = str2num(dims{1}) * str2num(dims{2}) * str2num(dims{3});
      surface = 2 * (str2num(dims{1}) * str2num(dims{2}) + ...
                     str2num(dims{2}) * str2num(dims{3}) + ...
                     str2num(dims{1}) * str2num(dims{3}));
      helpdlg(sprintf('Results:\nVolume = %.3f\nSurface = %.3f', volume, surface), 'Box Dimensions');
   end
   %-----------------------------------------------
   disp('- test inputdlg with prescribed vector [1,2,3] for # of lines per text field and defaults.');
   prompt = {'Width','Height','Depth'};
   default = {'1.10', '2.10', '3.10'};
   rc = [1,2,3];  % NOTE: must be an array
   dims = inputdlg ( prompt, 'Enter Box Dimensions',rc,default );
   if isempty(dims)
      helpdlg('Canceled by user', 'Information');
   else
      volume  = str2num(dims{1}) * str2num(dims{2}) * str2num(dims{3});
      surface = 2 * (str2num(dims{1}) * str2num(dims{2}) + ...
                     str2num(dims{2}) * str2num(dims{3}) + ...
                     str2num(dims{1}) * str2num(dims{3}));
      helpdlg(sprintf('Results:\nVolume = %.3f\nSurface = %.3f', volume, surface), 'Box Dimensions');
   end
   %-----------------------------------------------
   disp('- test inputdlg with prescribed row by column sizes and defaults.');
   prompt = {'Width','Height','Depth'};
   default = {'1.10', '2.20', '3.30'};
   rc = [1,10; 2,20; 3,30];  % NOTE: must be an array
   dims = inputdlg ( prompt, 'Enter Box Dimensions',rc,default );
   if isempty(dims)
      helpdlg('Canceled by user', 'Information');
   else
      volume  = str2num(dims{1}) * str2num(dims{2}) * str2num(dims{3});
      surface = 2 * (str2num(dims{1}) * str2num(dims{2}) + ...
                     str2num(dims{2}) * str2num(dims{3}) + ...
                     str2num(dims{1}) * str2num(dims{3}));
      helpdlg(sprintf('Results:\nVolume = %.3f\nSurface = %.3f', volume, surface), 'Box Dimensions');
   end
end

%% show a table of TeX symbol codes and the resulting Unicode character
function test_TeXCodes
   %-----------------------------------------------
   disp('- test TeX code to Unicode translation.');

   msgbox ( ['\\alpha  = ''\alpha ''      \\beta  = ''\beta ''      \\gamma  = ''\gamma ''', 10, ...
             '\\delta  = ''\delta ''      \\epsilon  = ''\epsilon ''      \\zeta  = ''\zeta ''', 10, ...
             '\\eta  = ''\eta ''      \\theta  = ''\theta ''      \\vartheta  = ''\vartheta ''', 10, ...
             '\\iota  = ''\iota ''      \\kappa  = ''\kappa ''      \\lambda  = ''\lambda ''', 10, ...
             '\\mu  = ''\mu ''      \\nu  = ''\nu ''      \\xi  = ''\xi ''', 10, ...
             '\\pi  = ''\pi ''      \\rho  = ''\rho ''      \\sigma  = ''\sigma ''', 10, ...
             '\\varsigma  = ''\varsigma ''      \\tau  = ''\tau ''      \\phi  = ''\phi ''', 10, ...
             '\\chi  = ''\chi ''      \\psi  = ''\psi ''      \\omega  = ''\omega ''', 10, ...
             '\\upsilon  = ''\upsilon ''      \\Gamma  = ''\Gamma ''      \\Delta  = ''\Delta ''', 10, ...
             '\\Theta  = ''\Theta ''      \\Lambda  = ''\Lambda ''      \\Pi  = ''\Pi ''', 10, ...
             '\\Xi  = ''\Xi ''      \\Sigma  = ''\Sigma ''      \\Upsilon  = ''\Upsilon ''', 10, ...
             '\\Phi  = ''\Phi ''      \\Psi  = ''\Psi ''      \\Omega  = ''\Omega ''', 10, ...
             '\\Im  = ''\Im ''      \\Re  = ''\Re ''      \\leq  = ''\leq ''', 10, ...
             '\\geq  = ''\geq ''      \\neq  = ''\neq ''      \\pm  = ''\pm ''', 10, ...
             '\\infty  = ''\infty ''      \\partial  = ''\partial ''      \\approx  = ''\approx ''', 10, ...
             '\\circ  = ''\circ ''      \\bullet  = ''\bullet ''      \\times  = ''\times ''', 10, ...
             '\\sim  = ''\sim ''      \\nabla  = ''\nabla ''      \\ldots  = ''\ldots ''', 10, ...
             '\\exists  = ''\exists ''      \\neg  = ''\neg ''      \\aleph  = ''\aleph ''', 10, ...
             '\\forall  = ''\forall ''      \\cong  = ''\cong ''      \\wp  = ''\wp ''', 10, ...
             '\\propto  = ''\propto ''      \\otimes  = ''\otimes ''      \\oplus  = ''\oplus ''', 10, ...
             '\\oslash  = ''\oslash ''      \\cap  = ''\cap ''      \\cup  = ''\cup ''', 10, ...
             '\\ni  = ''\ni ''      \\in  = ''\in ''      \\div  = ''\div ''', 10, ...
             '\\equiv  = ''\equiv ''      \\int  = ''\int ''      \\perp  = ''\perp ''', 10, ...
             '\\wedge  = ''\wedge ''      \\vee  = ''\vee ''      \\supseteq  = ''\supseteq ''', 10, ...
             '\\supset  = ''\supset ''      \\subseteq  = ''\subseteq ''      \\subset  = ''\subset ''', 10, ...
             '\\clubsuit  = ''\clubsuit ''      \\spadesuit  = ''\spadesuit ''      \\heartsuit  = ''\heartsuit ''', 10, ...
             '\\diamondsuit  = ''\diamondsuit ''      \\copyright  = ''\copyright ''      \\leftarrow  = ''\leftarrow ''', 10, ...
             '\\uparrow  = ''\uparrow ''      \\rightarrow  = ''\rightarrow ''      \\downarrow  = ''\downarrow ''', 10, ...
             '\\leftrightarrow  = ''\leftrightarrow ''      \\updownarrow  = ''\updownarrow '''], ...
             'TeX symbol code table Test');
end
