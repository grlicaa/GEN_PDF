CREATE OR REPLACE PACKAGE GEN_PDF AS


gen_pdf_version constant varchar2(10) := '1.0';
wpdf_version constant varchar2(10) := '1.4.8b'; 
as_pdf_version constant varchar2(10) := '3'; 

--- Start: Security Note: 
--- 
--- For security it's a good idea to restrict this package from being invoked from a url.
--- RESTRICT ACCESS TO THIS; http://mysite.com/plsdad/wpdf.[anything]
--- 

/*

log: 

             Andrej Grlica - andrej.grlica@gmail.com
14/10/2015 - changed font use from wpdf to as_pdf3 (load_ttf, set_font, flat_encode,)
           - changed making input in wpdf pdfDoc from clob to blob pdfBoc
           - changed option border and fill color to single cell in table           
                       

4/9/2014 - procedure can calculate the other dimension if only one is provided to keep the original proportion
4/8/2014 - fixed probelm with images when in landscape  
         - now ln can really accept a null

1/13/2014 - this is a fresh copy of the production code

6/30/2013 
  - added resetWpdf;

5/11/2013
  - add trRowSpace()

5/1/2013
  - added procedure fontSave; procedure fontRestore; added GetPageHeight();


12/16/2012
  - th now accepts the parameter ul (for underline).  Please note: the default is true, so set this to false as needed.  You may need 
    to update any PDFs already written
    

12/15/2012
  - fixed th issue on pages 2-n...hard to explain, it is better now
  - added new optional param to th: label_n   which will print on pages 2-n, in case you want page 2 to have a differnt table header than page 1
  - added new optional param to th: fox       which will print on pages 2-n, in case you want to print "(continued)" or text beside the th label  


11/15/2012
  - now when using the th/td procedures to create the table, the headers are now reprinted after the page break, This happens in MyRepetitiveHeader.
  

10/11/2012
  - added store address line 2 in headerStd()

9/26/2012
  - added code39 barcode printing function

8/18/2012
  - headerStd() now allows l1-l5 to split into columns with a ^ character.  Pass like l1=>'Column 1^Column2'
  - added hideHeader to th function.  Set 1st column hideHeader to true and the header row is not displayed
  - added procedure thUnder which underlines the header row
  - added sytle: text-decoration:underline
  - added sytle: font-style:italic
    
  
6/2/2012
  - Mitigated security risk which allowed a hacker to call any procedure from a url.
    By making paramTable tv4000a a required parameter in setHeaderProc() and setFooterProc() 
    this function can no longer be called from the url as ur params must be varchar2 or number only.
    
    

6/1/2012
  - added the ability to call setxy() before th() so that the table can be placed anywhere on the page.
    If you use setxy() to put 2 tables side by side, make sure either table will not break to new page.  
    This is a known limitation  To overcome I'd need to figure out how to write to prior page. So if either
    table may break to a new page, put the tables under one another instead of beside one another.

  - Found security bug: setHeaderProc(), setFooterProc(), Header(), buildPlsqlStatment() 
    allow a hacker to execute any pl/sql in the database
 

    
5-17-2012
  - fixed problem with AliasNbPages
  - fixed colspan problem
  - added new functions:
    
                              tdborder(b boolean);        -- turn border on for td 
                              tdFill(b boolean);          -- turns on fill for td 
                              tdLineHeight(h number);     -- set line height for td globally
                              tClear(ct in out number);   -- add ct (use as page ct)  when tclear is called set ct to 0
                              sh1 ( ct in out number);    -- accept ct and applies row level shading  -- 1st rows start shade
                              sh2 ( ct in out number);    -- accept ct and applies row level shading  -- 2nd row starts shade
                              sh ( ct in out number);     -- if border is on and fill is on call sh1 else call sh2
                                



WPDF -- Brian McGinity brian@databaseknowledge.com

The Story of WPDF:
I needed to generate PDFs from pl/sql so I downloaded and installed PL_FPDF v.0.9.3 (http://www.erasme.org/PL-FPDF,1337).  
The code worked great for small PDFs.  For large PDFs there were problems as each page was being stored in 32k variable.   

So I overloaded a few functions and rewrote a handful of procedures converting each page to a CLOB.  

Next I added functionality:
1.  The ability to return the pdf into a blob variable
2.  Multi-cell function which knows when page break 
3.  TH / TD functions to create tables
4.  Styles for:  align:left; align:right ;align:center; font-family, font-size, colspan, line-height

Then came images.  The image handling in PL_FPDF relies on functions only supported in Oracle Standard / Enterprise editions.  This meant 
that Oracle XE had problems.  So with the suggestion of another user, I added the code pertaining to images 
in AS_PDF (http://technology.amis.nl/2010/10/20/as_pdf-generating-a-pdf-document-with-some-plsql/) to WPDF.  This gave the ability for 
Oracle XE users to add images to PDFs.

Then another problem came up: certain .png images failed because of string overflow issues.  So I fixed this problem and things and things 
are working well.

So much of the success of WPDF belongs to the authors of PL-PDF and AS_PDF.   I’m using WPDF in production each day and have found the 
code to be stable.

Future code changes:
1.  PDF contains a Table of Contents like structure.  This structure is limited to 32k, so there still is a theoretical limit to how many types of things can be placed inside of pdf.  So far I have yet to reach this limit.  Someday I’ll convert this clobs as well.  
2.  Add more image processing types.  Currently limited to .jpg and .png


*/
 

fpdf_version constant varchar2(10) := '1.53'; 
subtype word is varchar2(80);

type tv4000a is table of varchar2(4000) index by word;

-- Constantes globales
noparam tv4000a;


procedure  resetWpdf;

-- as_pdf3 
procedure init_core_fonts;
function add_font( p_index pls_integer ) return number;
function add_object( p_txt varchar2 := null ) return number;
procedure txt2pdfdoc( p_txt varchar2 );
  procedure set_font
    ( p_index pls_integer
    , p_fontsize_pt number
    , p_output_to_doc boolean := true
    );
--
  function set_font
    ( p_fontname varchar2
    , p_fontsize_pt number
    , p_output_to_doc boolean := true
    )
  return pls_integer;
--
  procedure set_font
    ( p_fontname varchar2
    , p_fontsize_pt number
    , p_output_to_doc boolean := true
    );
--
  function set_font
    ( p_family varchar2
    , p_style varchar2 := 'N'
    , p_fontsize_pt number := null
    , p_output_to_doc boolean := true
    ) return pls_integer;
  procedure set_font
    ( p_family varchar2
    , p_style varchar2 := 'N'
    , p_fontsize_pt number := null
    , p_output_to_doc boolean := true
    );
  function load_ttf_font
    ( p_font blob
    , p_encoding varchar2 := 'WINDOWS-1252'
    , p_embed boolean := false
    , p_compress boolean := true
    , p_offset number := 1
    )
  return pls_integer;
--
  procedure load_ttf_font
    ( p_font blob
    , p_encoding varchar2 := 'WINDOWS-1252'
    , p_embed boolean := false
    , p_compress boolean := true
    , p_offset number := 1
    );
--
/*
  function load_ttf_font
    ( p_dir varchar2 := 'MY_FONTS'
    , p_filename varchar2 := 'BAUHS93.TTF'
    , p_encoding varchar2 := 'WINDOWS-1252'
    , p_embed boolean := false
    , p_compress boolean := true
    )
  return pls_integer;
--
  procedure load_ttf_font
    ( p_dir varchar2 := 'MY_FONTS'
    , p_filename varchar2 := 'BAUHS93.TTF'
    , p_encoding varchar2 := 'WINDOWS-1252'
    , p_embed boolean := false
    , p_compress boolean := true
    );
  procedure load_ttc_fonts
    ( p_dir varchar2 := 'MY_FONTS'
    , p_filename varchar2 := 'CAMBRIA.TTC'
    , p_encoding varchar2 := 'WINDOWS-1252'
    , p_embed boolean := false
    , p_compress boolean := true
    );
  procedure save_pdf
    ( p_dir varchar2 := 'MY_DIR'
    , p_filename varchar2 := 'myPDF.pdf'
    , p_freeblob boolean := true
    ) ;
    */        
    
--
  procedure load_ttc_fonts
    ( p_ttc blob
    , p_encoding varchar2 := 'WINDOWS-1252'
    , p_embed boolean := false
    , p_compress boolean := true
    );
--

function flate_encode( p_val blob )
  return blob;    
  function txt2raw( p_txt varchar2 )
  return varchar2; 

-- methods added to FPDF
function GetCurrentFontSize return number;
function GetCurrentFontStyle return varchar2;
function GetCurrentFontFamily return varchar2;
procedure SetDash(pblack number default 0, pwhite number default 0);
function GetLineSpacing return number;
Procedure SetLineSpacing (pls number);

-- FPDF public methods
procedure Ln(h number default null);
function GetXpx return number;
function  GetX return number;
procedure SetX(px number);
function GetYpx return number;
function  GetY return number;
procedure SetY(py number);
procedure SetXY(x number,y number);

function GetPageHeight return number;

procedure SetHeaderProc(headerprocname in varchar2, paramTable tv4000a);
procedure SetFooterProc(footerprocname in varchar2, paramTable tv4000a);
procedure SetMargins(left number,top number ,right number default -1);
procedure SetLeftMargin( pMargin number);
procedure SetTopMargin(pMargin number);
procedure SetRightMargin(pMargin number);
procedure SetAutoPageBreak(pauto boolean,pMargin number default 13);
procedure SetDisplayMode(zoom varchar2,layout varchar2 default 'continuous');
procedure SetCompression(p_compress boolean default true);
procedure SetTitle(ptitle varchar2);
procedure SetSubject(psubject varchar2);
procedure SetAuthor(pauthor varchar2);
procedure SetKeywords(pkeywords varchar2);
procedure SetCreator(pcreator varchar2);
procedure SetAliasNbPages(palias varchar2 default '{nb}');
procedure Header;
procedure Footer;
function  PageNo return number;
procedure SetDrawColor(r number,g number default -1,b number default -1);
procedure SetFillColor (r number,g number default -1,b number default -1);
procedure SetTextColor (r number,g number default -1,b number default -1);
procedure SetLineWidth(width number);
procedure Line(x1 number,y1 number,x2 number,y2 number);
procedure Rect(px number,py number,pw number,ph number,pstyle varchar2 default '');
function  AddLink return number;
procedure SetLink(plink number,py number default 0,ppage number default -1);
procedure Link(px number,py number,pw number,ph number,plink varchar2);
procedure Text(px number,py number,ptxt varchar2, p_degrees_rotation number default null);
function  AcceptPageBreak return boolean;


procedure fontSave;
procedure fontRestore;

function GetStringWidth(pstr varchar2) return number;
procedure SetFontSize(psize number);
procedure Cell
         (pw number,
          ph number default 0,
          ptxt varchar2 default '',
          pborder varchar2 default '0',
          pln number default 0,
          palign varchar2 default '',
          pfill number default 0,
          plink varchar2 default '');
procedure MultiCell
  ( pw number,
    ph number default 0,
    ptxt varchar2,
    pborder varchar2 default '0',
    palign varchar2 default 'J',
    pfill number default 0,
    phMax number default 0);

procedure Write(pH varchar2,ptxt varchar2,plink varchar2 default null);



/*
procedure image ( pFile varchar2, 
                                    imageBlob     blob,
                  pX number, 
                  pY number, 
                  pWidth number default 0,
                  pHeight number default 0,
                  pType varchar2 default null,
                  pLink varchar2 default null);
*/


procedure image ( imageBlob         blob,
                    px                          number,
                    py                         number,
                    p_width             number default 0,
                    p_height             number default 0,
                    pLink                 varchar2 default null
                  );


procedure image ( imageClob         clob,
                    px                          number,
                    py                         number,
                    p_width             number default 0,
                    p_height             number default 0,
                    pLink                 varchar2 default null
               );

function base64clob_to_blob (p_clob_in in clob) return blob;


procedure code39(xpos number, ypos number, code varchar2, bottomtext varchar2 default null, baseline number default 0.5, height number default 5);


procedure Output(pname varchar2 default null,pdest varchar2 default null);
function Output     return blob;

procedure OpenPDF;
procedure ClosePDF;
procedure AddPage(orientation varchar2 default '');
procedure pdf  (orientation varchar2 default 'P', unit varchar2 default 'mm', format varchar2 default 'A4');
--procedure Error(pmsg varchar2);
procedure DebugEnabled;
procedure DebugDisabled;
function GetScaleFactor return number;

procedure jsSet(theJs varchar2);
procedure jsAutoPrint(silent boolean default false,  closeWindow boolean default false);


procedure thUnder;
procedure tdborder(b boolean);
procedure tdFill(b boolean);

procedure tdLineHeight(h number);
procedure trRowSpace(h number);


procedure tEnd;
procedure tClear;
procedure tClear(ct in out number);

procedure sh1 ( ct in out number);
procedure sh2 ( ct in out number);
procedure sh ( ct in out number);

procedure thDraw;
    
procedure th(
              width       number, 
              label       varchar2, 
              align       varchar2 default 'c', 
              hideHeader  boolean default false, 
              label_n     varchar2 default null, 
              fox         varchar2 default null,
              ul          boolean  default true
              );


procedure td(style varchar2, d varchar2);


function getCenterX( s varchar2) return number;

procedure headerStd(
                        store_name        varchar2,
                        store_addr_1    varchar2,
                        store_addr_2  varchar2,
                        store_city        varchar2,
                        store_state        varchar2,
                        store_zip            varchar2,
                        store_phone        varchar2,
                        t                            varchar2,  
                        nbrLabel            varchar2,  
                        nbr                     varchar2,  
                        dt                         varchar2,  
                        l1                         varchar2,  
                        l2                         varchar2 default null, 
                        l3                         varchar2 default null, 
                        l4                         varchar2 default null,                        
                        r1                         varchar2,  
                        r2                         varchar2 default null, 
                        r3                         varchar2 default null, 
                        r4                         varchar2 default null,
                        r5                         varchar2 default null,
                        barcode       varchar2 default null,                  
                        bc_x          number   default 182,
                        bc_y          number   default 40,
                        bc_label      varchar2 default 'smurf',
                        dtLabel       varchar2 default 'Date' 
                      );

procedure headerLandscape(
                            header_1    varchar2,
                            header_2    varchar2 default null,
                            header_3    varchar2 default null,
                            header_4    varchar2 default null
                          );
function getFileFromDatabase(pFile varchar2) return blob;
procedure pdf_error;

function str_len( p_txt in varchar2 )
  return number;
  

--
-- Sample codes.
--

procedure MyRepetitiveHeader(param1 varchar2, param2 varchar2);
procedure MyRepetitiveFooter(param1 varchar2);
procedure testHeader;

    
procedure emp_demo;
procedure fonts_demo;
procedure barcode_demo;
procedure rotate_demo;
procedure images_demo;
procedure chart_demo;


END;
/