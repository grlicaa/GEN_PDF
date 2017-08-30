CREATE OR REPLACE PACKAGE BODY GEN_PDF AS

-- as3
  type tp_pls_tab is table of pls_integer index by pls_integer;
  type tp_font is record
    ( standard boolean
    , family varchar2(100)
    , style varchar2(2)  -- N Normal
                         -- I Italic
                         -- B Bold
                         -- BI Bold Italic
    , subtype varchar2(15)
    , name varchar2(100)
    , fontname varchar2(100)
    , char_width_tab tp_pls_tab
    , encoding varchar2(100)    , charset varchar2(1000)
    , compress_font boolean := true
    , fontsize number
    , unit_norm number
    , bb_xmin pls_integer
    , bb_ymin pls_integer
    , bb_xmax pls_integer
    , bb_ymax pls_integer
    , flags pls_integer
    , first_char pls_integer
    , last_char pls_integer
    , italic_angle number
    , ascent pls_integer
    , descent pls_integer
    , capheight pls_integer
    , stemv pls_integer
    , diff varchar2(32767)
    , cid boolean := false
    , fontfile2 blob
    , ttf_offset pls_integer
    , used_chars tp_pls_tab
    , numGlyphs pls_integer
    , indexToLocFormat pls_integer
    , loca tp_pls_tab
    , code2glyph tp_pls_tab
    , hmetrics tp_pls_tab
    );
type tp_font_tab is table of tp_font index by pls_integer;
g_fonts tp_font_tab;
g_used_fonts tp_pls_tab;
g_current_font pls_integer;
c_nl constant varchar2(2) := chr(10);
type tp_objects_tab is table of number(10) index by pls_integer;


-- fpdf

gb_mode_debug boolean := true;
 
-- Privates types
subtype flag         is boolean;
subtype car         is varchar2(2);
subtype phrase     is varchar2(32767);
subtype txt         is varchar2(32767);
subtype bigtext is varchar2(32767);
subtype margin     is number;

-- type tv1 is table of varchar2(1) index by binary_integer;
type tbool     is table of boolean index by binary_integer;
type tn         is table of number index by binary_integer;
type tv4000 is table of varchar2(4000) index by binary_integer;
type tvclob     is table of clob  index by binary_integer;

type tp_img is record(
                          n number,                      -- resource object nbr 
                          i number,                        -- image number for x,y placement
                          adler32 varchar2( 8 ),
                          width pls_integer,
                          height pls_integer,
                          color_res pls_integer,
                          color_tab raw( 768 ),
                          greyscale boolean,
                          pixels blob,
                          type varchar2( 5 ),
                          nr_colors pls_integer
                        );

type imagesArray is table of tp_img  index by varchar2(8);

type recFormat is record ( largeur number, hauteur number);
type rec2chp is record ( zero txt, un txt);

type rec5 is record ( zero txt,
                      un txt,
                      deux txt,
                      trois txt,
                      quatre txt);

type LinksArray is table of rec5;

type Array2dim is table of rec2chp;


-- Private properties
 images imagesArray;

 page number;               -- current page number
 n number;                  -- current object number
 offsets tv4000;            -- array of object offsets
 pdfBoc blob := null;                 -- buffer holding in-memory final PDF document.
 imgBlob blob;              -- allows creation of persistent blobs for images
 pages tvclob;               -- array containing pages
 state word;                -- current document state
 b_compress flag := true;  -- compression flag
 DefOrientation car;        -- default orientation
 CurOrientation car;        -- current orientation
 OrientationChanges tbool;    -- array indicating orientation changes
 k number;                  -- scale factor (number of points in user unit)
 fwPt number;
 fhPt number;                 -- dimensions of page format in points
 fw number;
 fh number;                 -- dimensions of page format in user unit
 wPt number;
 hPt number;                   -- current dimensions of page in points
 w number;
 h number;                   -- current dimensions of page in user unit
 lMargin margin;            -- left margin
 tMargin margin;            -- top margin
 rMargin margin;            -- right margin
 bMargin margin;            -- page break margin
 cMargin margin;            -- cell margin
 x number;
 y number;                   -- current position in user unit for cell positioning
 lasth number;              -- height of last cell printed
 LineWidth number;          -- line width in user unit
 --images imagesArray;                 -- array of used images
 PageLinks LinksArray;          -- array of links in pages
 links Array2dim;              -- array of internal links
 FontFamily word;             -- current font family
 FontStyle word;              -- current font style

 fontFamilySave   word;           -- current font family
 fontStyleSave    word;            -- current font style
 fontSizePtSave   number;
 

 underline flag;              -- underlining flag
 FontSizePt number;         -- current font size in points
 FontSize number;           -- current font size in user unit
 DrawColor phrase;          -- commands for drawing color
 FillColor phrase;          -- commands for filling color
 TextColor phrase;          -- commands for txt color
 ColorFlag flag;              -- indicates whether fill and txt colors are different
 ws word;                     -- word spacing
 AutoPageBreak flag;          -- automatic page breaking
 PageBreakTrigger number;   -- threshold used to trigger page breaks
 InFooter flag;               -- flag set when processing footer
 ZoomMode word;               -- zoom display mode
 LayoutMode word;             -- layout display mode
 title txt;                  -- title
 subject txt;                -- subject
 author txt;                 -- author
 keywords txt;               -- keywords
 creator txt;                -- creator
 AliasNbPages     word  := '`p';           -- alias for total number of pages
 PDFVersion       word;             -- PDF version number

    jsIncluded        boolean    := false;
    jsNbr                    number;
     jsStr                    varchar2(4000);
 
  -- Propriet�s ajout�es lors du portage en PLSQL.
  MyHeader_Proc txt;                        -- Personal Header procedure.
  MyHeader_ProcParam tv4000a;            -- Table of parameters of the personal header Proc.
  MyFooter_Proc txt;                        -- Personal Footer procedure.
  MyFooter_ProcParam tv4000a;            -- Table of parameters of the personal footer Proc.
  formatArray recFormat;                    -- Dimension of the format (variable : format).
  Linespacing number;

    -- variables dont je ne maitrise pas bien l'emploi.
     -- A v�rifier au court de la validation du portage.
    originalsize word;
  size1 word;
  size2 word;


type recImage is record ( n number,                -- indice d'insertion dans le document
                          i number,              -- ?
                          w number,             -- width
                          h number,             -- height
                          cs txt,            -- colorspace
                          bpc txt,           -- Bit per color
                          f txt,              -- File Format
                          parms txt,         -- pdf parameter for this image
                          pal txt,           -- colors palette informations 
                          trns tn,            -- transparency 
                          data blob            -- Data
 );


type imagesArrayOrd is table of recImage index by txt;

type headerRec is record ( 
                      x               number,
                      y               number,
                      width                     number,
                      label                        varchar2(200),
                      label_n         varchar2(200),    -- for 2 - N page label 
                      fox             varchar2(200),    -- for 2 - N page label fox
                                          colspan                    number,
                      colspanwidth        number,
                      hdr_fontFamily  varchar2(15),
                      hdr_fontStyle   varchar2(3),
                      hdr_fontSize    varchar2(10),
                      hdr_align       varchar2(10),
                      hdr_ul          boolean,
                      hideHeader      boolean,
                      fontFamily            varchar2(15),
                      fontStyle             varchar2(3),
                      fontSize                varchar2(10),
                                      align                        varchar2(10),
                                      lineheight            number,
                      border          varchar2(10),
                      fill          varchar2(10),
                      
                            data                        varchar2(32727)
                    );

type headerArray is table of headerRec index by pls_integer;

headers               headerArray;
inTh                boolean := false;
tdCt                      pls_integer;
tdIdCurrent         pls_integer;
tdIdxLast           pls_integer;
tdLineHeightGlobal  number  := 5.5;
trRowSpaceGlobal    number  := 0;
tdBorderGlobal      varchar2(1) default 0;
tdFillGlobal        binary_integer default 0;
thUnderLine         boolean := false;
tdSavePos           boolean := false;

/*******************************************************************************
*                                                                              *
*           Protected methods : Internal function and procedures               *
*                                                                              *
*******************************************************************************/



function clobfromblob(p_blob blob) return clob is
      l_clob         clob;
      l_dest_offsset integer := 1;
      l_src_offsset  integer := 1;
      l_lang_context integer := dbms_lob.default_lang_ctx;
      l_warning      integer;

   begin

      if p_blob is null then
         return null;
      end if;

      dbms_lob.createTemporary(lob_loc => l_clob
                              ,cache   => false);

      dbms_lob.converttoclob(dest_lob     => l_clob
                            ,src_blob     => p_blob
                            ,amount       => dbms_lob.lobmaxsize
                            ,dest_offset  => l_dest_offsset
                            ,src_offset   => l_src_offsset
                            ,blob_csid    => dbms_lob.default_csid
                            ,lang_context => l_lang_context
                            ,warning      => l_warning);

      return l_clob;

   end;

FUNCTION c2b (p_clob IN CLOB)
   RETURN BLOB
IS
   v_blob          BLOB;
   v_offset        NUMBER DEFAULT 1;
   v_amount        NUMBER DEFAULT 4096;
   v_offsetwrite   NUMBER DEFAULT 1;
   v_amountwrite   NUMBER;
   v_buffer        VARCHAR2 (4096 CHAR);
BEGIN
   DBMS_LOB.createtemporary (v_blob, TRUE);

   BEGIN
      LOOP
         DBMS_LOB.READ (lob_loc   => p_clob,
                        amount    => v_amount,
                        offset    => v_offset,
                        buffer    => v_buffer);

         v_amountwrite :=
            UTL_RAW.LENGTH (r => UTL_RAW.cast_to_raw (c => v_buffer));

         DBMS_LOB.WRITE (lob_loc   => v_blob,
                         amount    => v_amountwrite,
                         offset    => v_offsetwrite,
                         buffer    => UTL_RAW.cast_to_raw (v_buffer));

         v_offsetwrite := v_offsetwrite + v_amountwrite;

         v_offset := v_offset + v_amount;
         v_amount := 4096;
      END LOOP;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         NULL;
   END;

   RETURN v_blob;
END; 
  


procedure  resetWpdf is
begin

  images.delete;
  page            := null;
  n               := null;
  
  offsets.delete; 
  pdfBoc          := null;
  imgBlob         := null;
  pages.delete; 
  state           := null;
  b_compress      := true;  
  DefOrientation  := null;
  CurOrientation  := null;
  OrientationChanges.delete; 
  k               := null;
  fwPt            := null;
  fhPt            := null;
  fw              := null;
  fh              := null;
  wPt             := null;
  hPt             := null;
  w               := null;
  h               := null;
  lMargin         := null;
  tMargin         := null;
  rMargin         := null;
  bMargin         := null;
  cMargin         := null;
  x               := null;
  y               := null;
  lasth           := null;
  LineWidth       := null;
  
  PageLinks       := null;
  links           := null;
  FontFamily      := null;
  FontStyle       := null;
  
  fontFamilySave   := null;
  fontStyleSave    := null;
  fontSizePtSave   := null;
   

  underline         := null;
  FontSizePt        := null;
  FontSize          := null;
  DrawColor         := null;
  FillColor         := null;
  TextColor         := null;
  ColorFlag         := null;
  ws                := null;
  AutoPageBreak     := null;
  PageBreakTrigger  := null;

  InFooter        := null;
  ZoomMode        := null;
  LayoutMode      := null;
  title           := null;
  subject         := null;
  author          := null;
  keywords        := null;
  creator         := null;
  
  jsIncluded      := false;
  jsNbr          := null;
  jsStr          := null;
   
 
  MyHeader_ProcParam.delete;  
  MyFooter_ProcParam.delete;
  MyHeader_Proc         := null;

  MyFooter_Proc         := null;
  formatArray           := null;
  Linespacing           := null;
  
  originalsize          := null;
  size1                 := null;
  size2                 := null;

  
  headers.delete;
  inTh                := false;
  tdCt                := null;
  tdIdCurrent         := null;
  tdIdxLast           := null;
  tdLineHeightGlobal  := 5.5;
  trRowSpaceGlobal    := 0;
  tdBorderGlobal      := 0;
  tdFillGlobal        := 0;
  thUnderLine         := false;
  tdSavePos           := false;
    

end;


function posof(str varchar2, pos number, d varchar2 default '_') return varchar2  is
  s number;
  e number;
begin

  if pos = 1 then
    return substr(str, 1, instr(str, d,1)-1);
  end if;
  
  s := instr(str||d, d, 1,     pos-1)+1;
  e := instr(str||d, d, s);

  return substr(str||d, s, (e-s) );

end;


function nvl_nbsp(inval varchar2) return varchar2 is
begin

  return nvl(trim(inval), '&'||'nbsp');

end;

procedure htpc (z clob) is
  v_read_amount     integer   := 32767;
  v_read_offset     integer   := 1;
  v_buffer          varchar2(32767);
begin


  begin
    loop
      dbms_lob.read(z,v_read_amount,v_read_offset,v_buffer);
      htp.prn(v_buffer);
      v_read_offset := v_read_offset + v_read_amount;
      v_read_amount := 32767;
    end loop;

  exception
  when no_data_found then
     null;
  end;

  exception
  when others then
    null; --showHTMLdoc(p_id);

end;


procedure print (pstr varchar2) is
begin
  -- Choose the output mode...
  htp.p(pstr);
  -- My outpout method
  -- affiche.p(pstr);
end print;


----------------------------------------------------------------------------------
-- Testing if method for additionnal fonts exists in this package
-- lv_existing_methods MUST reference all the "p_put..." procedure of the package.
----------------------------------------------------------------------------------
function methode_exists(pMethodName varchar2) return boolean is
lv_existing_methods varchar2(2000)
:= 'p_putstream,p_putxobjectdict,p_putresourcedict,p_putimages,p_putresources,'||
   'p_putinfo,p_putcatalog,p_putheader,p_puttrailer,p_putpages';
begin
   if (instr(lv_existing_methods, lower(pMethodName) ) > 0 ) then
     return true;
   end if;
   return false;
exception
  when others then
   return false;
end methode_exists;

  function str_len( p_txt in varchar2 )
  return number
  is
    t_width number;
    t_char pls_integer;
    t_rtxt raw(32767);
    t_tmp number;
    t_font tp_font;
  begin
    if p_txt is null
    then
      return 0;
    end if;
--
    t_width := 0;
    t_font := g_fonts( g_current_font );
    if t_font.cid
    then
      t_rtxt := utl_raw.convert( utl_raw.cast_to_raw( p_txt )
                               , 'AMERICAN_AMERICA.AL16UTF16' -- 16 bit font => 2 bytes per char
                               , sys_context( 'userenv', 'LANGUAGE' )  -- ???? font characterset ?????
                               );
      for i in 1 .. utl_raw.length( t_rtxt ) / 2
      loop
        t_char := to_number( utl_raw.substr( t_rtxt, i * 2 - 1, 2 ), 'xxxx' );
        if t_font.flags = 4 -- a symbolic font
        then
-- assume code 32, space maps to the first code from the font
          t_char := t_font.code2glyph.first + t_char - 32;
        end if;
        if (   t_font.code2glyph.exists( t_char )
           and t_font.hmetrics.exists( t_font.code2glyph( t_char ) )
           )
        then
          t_tmp := t_font.hmetrics( t_font.code2glyph( t_char ) );
        else
          t_tmp := t_font.hmetrics( t_font.hmetrics.last() );
        end if;
        t_width := t_width + t_tmp;
      end loop;
      t_width := t_width * t_font.unit_norm;
   
    else
      t_rtxt := utl_raw.convert( utl_raw.cast_to_raw( p_txt )
                               , t_font.charset  -- should be an 8 bit font
                               , sys_context( 'userenv', 'LANGUAGE' )
                               );
      for i in 1 .. utl_raw.length( t_rtxt )
      loop
        t_char := to_number( utl_raw.substr( t_rtxt, i, 1 ), 'xx' );
        t_width := t_width + t_font.char_width_tab( t_char );
      end loop;
    end if;
    return t_width;
  end;

----------------------------------------------------------------------------------
-- Calculate the length of the final document contained in the plsql table pdfDoc.
----------------------------------------------------------------------------------
function getPDFDocLength return pls_integer is
begin
    return dbms_lob.getlength(pdfBoc);

end getPDFDocLength;



function getFileFromDatabase(pFile varchar2) return blob is
    b     blob;
    b2 blob;
  m    varchar2(30);
begin

    begin
        for c1 in (select workspace_id
                       from apex_workspaces) loop
        apex_util.set_security_group_id( c1.workspace_id );
         exit;
        end loop;
    end;

    
    begin
        SELECT BLOB_CONTENT INTO b 
          FROM WWV_FLOW_FILES 
         WHERE ID = (SELECT MAX(ID) 
                       FROM WWV_FLOW_FILES 
                      WHERE upper(filename)=upper(pFile));
                      
    exception  when others then   raise_application_error(-20100,'Incorrect picture name on getImageFromDatabase: ');
    end;
    
  return b;

end getFileFromDatabase;


--------------------------------------------------------------------------------
-- Enables debug infos
--------------------------------------------------------------------------------
procedure DebugEnabled is
begin
  gb_mode_debug := true;
end DebugEnabled;

--------------------------------------------------------------------------------
-- disables debug infos
--------------------------------------------------------------------------------
procedure DebugDisabled is
begin
  gb_mode_debug := false;
end DebugDisabled;

--------------------------------------------------------------------------------
-- Returns the k property
--------------------------------------------------------------------------------
function GetScaleFactor return number is
begin
    -- Get scale factor
    return k;
end GetScaleFactor;

--------------------------------------------------------------------------------
-- Returns the Linespacing property
--------------------------------------------------------------------------------
function GetLineSpacing return number is
begin
    -- Get LineSpacing property
    return LineSpacing;
end GetLineSpacing;

--------------------------------------------------------------------------------
-- sets the Linespacing property
--------------------------------------------------------------------------------
Procedure SetLineSpacing (pls number) is
begin
    -- Set LineSpacing property
    LineSpacing := pls;
end SetLineSpacing;

----------------------------------------------------------------------------------
-- Compatibilit� PHP -> PLSQL : proc. and func. sp�cifiques au portages
--                                     ajout�e pour des facilit�s de traduction
----------------------------------------------------------------------------------
function empty (p_myvar varchar2) return boolean is
begin
  if (p_myvar is null) then
    return true;
  end if;
  return false;
end empty;

function empty (p_mynum number) return boolean is
begin
  return empty (p_myvar => to_char(p_mynum));
end empty;



function str_replace_clob (replacestr in varchar2, replacewith in varchar2, srcclob in clob ) return clob  is          
  
  l_buffer varchar2 (32767);
  l_amount binary_integer := 32767;
  l_pos integer := 1;
  l_clob_len integer;
  newclob clob := empty_clob;
      
begin 


  dbms_lob.createtemporary( newclob, true ); 
  l_clob_len := dbms_lob.getlength (srcclob);
  while l_pos < l_clob_len loop
   
    dbms_lob.read (srcclob,l_amount,l_pos,l_buffer);
     if l_buffer is not null then
      l_buffer := replace(l_buffer,replacestr,replacewith);
      dbms_lob.writeappend(newclob, length(l_buffer), l_buffer);
     end if;
     
     l_pos :=   l_pos + l_amount;
     
  end loop;
   
  return newclob;
   
end;


function str_replace ( psearch varchar2, preplace varchar2, psubject varchar2) return varchar2 is
begin
  return replace(psubject, psearch, preplace);
end str_replace;

function strlen (pstr varchar2) return number is
begin
  return length(pstr);
end strlen;

function strlen (pstr clob) return number is
begin
  return  DBMS_LOB.GETLENGTH (pstr);
end strlen;


function tonumber(v_str in varchar2) return number is
   v_num number;
   v_str2 varchar2(255);
begin
   begin
      v_num := to_number(v_str, '999.999');
   exception
      when others then
         v_num := null;
   end;
   if v_num is null then
      -- maybe wrong NLS, try again
      v_str2 := replace(v_str,',.','.,');
      begin
         v_num := to_number(v_str2, '999.999');
      exception
         when others then
            v_num := null;
      end;
   end if;
   return v_num;
end;

function tochar(pnum number, pprecision number default 2) return varchar2 is
mynum word := replace(to_char(pnum),',','.');
ceilnum word;
decnum word;
begin
  if (instr(mynum,'.') = 0) then
    mynum := mynum || '.0';
  end if;
  ceilnum := nvl(substr(mynum,1,instr(mynum,'.')-1), '0');
  decnum := nvl(substr(mynum,instr(mynum,'.')+1), '0');
  decnum := substr(decnum,1, pprecision);
  if (pprecision = 0 ) then
       mynum := ceilnum;
  else
       mynum := ceilnum || '.' ||decnum;
  end if;
  return mynum;
end tochar;

function date_YmdHis (p_date date default sysdate) return varchar2 is
begin
  return to_char(p_date,'YYYYMMDDHH24MISS');
end date_YmdHis;

function is_string (pstr varchar2) return boolean is
temp varchar2(2000);
begin
  temp := to_number(pstr);
  -- Si on passe l� c'est que la variable contient un nombre sinon => exception.
  return false;
exception
when others then
  return true;
end is_string;

function function_exists (pname varchar2) return boolean is
begin
  -- Pas de fct fdt de compression zlib sous oracle.
  return false;
end function_exists;

function strtoupper (pstr in out varchar2) return varchar2 is
begin
     return upper(pstr);
end strtoupper;

function strtolower (pstr in out varchar2) return varchar2 is
begin
     return lower(pstr);
end strtolower;

function substr_count (ptxt varchar2, pstr varchar2) return number is
  nbr number := 0;
begin
  for i in 1..length(ptxt)
  loop
    if (substr(ptxt,i,1) = pstr) then
      nbr := nbr + 1;
    end if;
  end loop;
  return nbr;
end substr_count;



procedure p_out(pstr varchar2 default null, pCRLF boolean default true) is

lv_CRLF varchar2(2) := null;

begin

  if (pCRLF) then
      lv_CRLF := chr(10);
    end if;

    -- Add a line to the document
    if(state = 2) then
        pages(page):= concat(pages(page) ,  pstr || lv_CRLF);
    else
        dbms_lob.append(pdfBoc, utl_raw.cast_to_raw(pstr||lv_CRLF)); 
    end if;
  
end p_out;


procedure p_out(pstr clob, pCRLF boolean default true) is
lv_CRLF varchar2(2) := null;
begin
  if (pCRLF) then
      lv_CRLF := chr(10);
    end if;
    -- Add a line to the document
    if(state = 2) then
        pages(page):= concat(pages(page), pstr || lv_CRLF);
    else
        dbms_lob.append(pdfBoc, utl_raw.cast_to_raw(pstr||lv_CRLF)); 
    end if;
end p_out;


procedure p_newobj is
begin
    -- Begin a new object
    n := n + 1;
    offsets(n) := getPDFDocLength();
    p_out(n || ' 0 obj');
--exception
--  when others then
--   error('p_newobj : '||sqlerrm);
end p_newobj;

----------------------------------------------------------------------------------------
function p_escape(pstr varchar2) return varchar2 is
begin
  return str_replace(')','\)',str_replace('(','\(',str_replace('\\','\\\\',pstr)));
end p_escape;

----------------------------------------------------------------------------------------
function p_textstring(pstr varchar2) return varchar2 is
begin
    -- Format a text string
    return '(' || p_escape(pstr) || ')';
end p_textstring;

----------------------------------------------------------------------------------------
procedure p_putstream(pstr varchar2) is
begin
    p_out('stream');
    p_out(pstr, false);
    p_out('endstream');
end p_putstream;


procedure p_putstream(pstr clob) is
begin
    p_out('stream');
    p_out(pstr, false);
    p_out('endstream');
end p_putstream;


procedure p_putstream(pData in blob) is
          c               clob :='';
          srcOffset       number := 1;
          dstOffset       number := 1;
          warning         number := 0;
          lang            PLS_INTEGER := 871;
begin
    p_out('stream');
        dbms_lob.append(pdfBoc, pData);            
    p_out(chr(10), false);
    p_out('endstream');
end p_putstream;


procedure p_putxobjectdict is
v txt;
begin
   v := images.first;
   while (v is not null) loop
         p_out('/I' || images(v).i || ' ' || images(v).n || ' 0 R');
         v := images.next(v);
   end loop;


end p_putxobjectdict;

----------------------------------------------------------------------------------------
procedure p_putresourcedict is
v varchar2(200);
    t_ind pls_integer;
    t_self number(10);
    t_fonts tp_objects_tab;
begin
    t_ind := g_used_fonts.first;
    while t_ind is not null
    loop
      t_fonts( t_ind ) := add_font( t_ind );
      t_ind := g_used_fonts.next( t_ind );
    end loop;    
--
    offsets(2):= getPDFDocLength();
    p_out('2 0 obj');
    p_out('<<');
    txt2pdfdoc( '/ProcSet [/PDF /Text /ImageB /ImageC /ImageI]' );
--
    if g_used_fonts.count() > 0
    then
      txt2pdfdoc( '/Font <<' );
      t_ind := g_used_fonts.first;
      while t_ind is not null
      loop
        txt2pdfdoc( '/F'|| to_char( t_ind ) || ' '
                  || to_char( t_fonts( t_ind ) ) || ' 0 R'
                  );
        t_ind := g_used_fonts.next( t_ind );
      end loop;
      txt2pdfdoc( '>>' );
    end if;
    p_out('/XObject <<');
    p_putxobjectdict();
    p_out('>>');
end p_putresourcedict;



procedure p_putimages2 is

  v             txt;
  p_img     tp_img;

begin

    v := images.first;
    while (v is not null)  loop
        p_newobj();
        
    images(v).n := n;
    p_img  := images(v);

        p_out('<</Type /XObject');
        p_out('/Subtype /Image');
        p_out('/Width '   || p_img.width);
        p_out('/Height '  || p_img.height);

    if p_img.color_tab is null then
      if p_img.greyscale  then
        p_out('/ColorSpace /DeviceGray' );
      else
        p_out('/ColorSpace /DeviceRGB' );
      end if;
    else
      p_out('/ColorSpace [/Indexed /DeviceRGB ' || to_char( utl_raw.length( p_img.color_tab ) / 3 - 1 ) || ' ' || to_char(n+1) || ' 0 R]' );
    end if;
        p_out('/BitsPerComponent ' || to_char( p_img.color_res));            

        if p_img.type = 'jpg' then
      p_out('/Length ' || dbms_lob.getlength(p_img.pixels) || ' /Filter /DCTDecode >>');
      p_putstream(p_img.pixels);
            p_out('/Filter /DCTDecode');
          p_out('/Length ' ||dbms_lob.getlength(p_img.pixels)|| '>>');
      p_putstream(p_img.pixels);
    elsif p_img.type = 'png' then
            p_out('/Filter /FlateDecode');
          p_out('/DecodeParms <</Predictor 15 /Colors '||p_img.nr_colors||' /BitsPerComponent ' || p_img.color_res  || ' /Columns ' || p_img.width || '>>');
          p_out('/Length ' ||dbms_lob.getlength(p_img.pixels)|| '>>');
      p_putstream(p_img.pixels);
    else
      p_putstream( p_img.pixels);
    end if;

        p_out('endobj');
    
    if p_img.color_tab is not null   then

            p_out('<< /Length ' || strlen(p_img.color_tab) || '>>');
            p_putstream(p_img.color_tab);
            p_out('endobj');
    end if;


        v := images.next(v);
    end loop;
  
end p_putimages2;


procedure jsSet(theJs varchar2) is
begin

    jsIncluded    := true;
    jsStr                := theJs;
  
end;


procedure jsAutoPrint(silent boolean default false,  closeWindow boolean default false) is
    printArgs  varchar2(130);
begin

    -- note about silent printing:  adobe just changed how silent printing works, 
  --                                                             there is a way to do it...i will code it soon
    --
    -- update: 4/8/2012 -- js printing may require each user to add a file in their pdf intstalled dir
  --                     this makes auto printing hard

    if silent then printArgs := '{bUI: true, bSilent: true}'; end if;
  
    --
  -- to close window, create closeit.html put in root dir:
  -- closeit.html: <html><head><script>window.close();</script></head></html>
    --
  
  if closeWindow then
        jsSet('this.print('||printArgs||'); this.getURL("/closeit.html", false);');
    else
        jsSet('this.print('||printArgs||');');
  end if;
  
  
end;


procedure jsAddTodoc is
begin

    p_newobj();
  jsNbr := n;
    p_out('<<');
  p_out('/Names [(EmbeddedJS) '|| to_char(n+1) ||' 0 R]');
    p_out('>>');
    p_out('endobj');

    p_newobj();
    p_out('<<');
    p_out('/S /JavaScript');
  p_out('/JS '||p_textstring(jsStr));
  p_out('>>');
    p_out('endobj');

end;

----------------------------------------------------------------------------------------
procedure p_putresources is
begin
    p_putimages2();

  if jsIncluded then jsAddTodoc; end if;

    -- Resource dictionary
    
    p_putresourcedict(); -- + fonts
    p_out('>>');
    p_out('endobj');

end p_putresources;

----------------------------------------------------------------------------------------
procedure p_putinfo is
begin
    p_out('/Producer ' || p_textstring('GEN PDF '||gen_pdf_version||' (Wpdf Pro ' || WPDF_VERSION||' + AS_PDF3)' ));
    if(not empty(title)) then
        p_out('/Title ' || p_textstring(title));
    end if;
    if(not empty(subject)) then
        p_out('/Subject ' || p_textstring(subject));
    end if;
    if(not empty(author)) then
        p_out('/Author ' || p_textstring(author));
    end if;
    if(not empty(keywords)) then
        p_out('/Keywords ' || p_textstring(keywords));
    end if;
    if(not empty(creator)) then
        p_out('/Creator ' || p_textstring(creator));
    end if;
    p_out('/CreationDate ' || p_textstring('D:' || date_YmdHis()));
end p_putinfo;

----------------------------------------------------------------------------------------
procedure p_putcatalog is
begin
    p_out('/Type /Catalog');
    p_out('/Pages 1 0 R');

    if(ZoomMode='fullpage') then
        p_out('/OpenAction [3 0 R /Fit]');
    elsif(ZoomMode='fullwidth') then
        p_out('/OpenAction [3 0 R /FitH null]');
    elsif(ZoomMode='real') then
        p_out('/OpenAction [3 0 R /XYZ null null 1]');
    elsif(not is_string(ZoomMode)) then
        p_out('/OpenAction [3 0 R /XYZ null null ' || (ZoomMode/100) || ']');
    end if;
    if(LayoutMode='single') then
        p_out('/PageLayout /SinglePage');
    elsif(LayoutMode='continuous') then
        p_out('/PageLayout /OneColumn');
    elsif(LayoutMode='two') then
        p_out('/PageLayout /TwoColumnLeft');
  end if;

  if jsIncluded then p_out('/Names <</JavaScript '||jsNbr||' 0 R>>'); end if;

end p_putcatalog;


----------------------------------------------------------------------------------------
procedure p_putheader is
begin
    p_out('%PDF-' || PDFVersion);
end p_putheader;


----------------------------------------------------------------------------------------
procedure p_puttrailer is
begin
    p_out('/Size ' || (n+1));
    p_out('/Root ' || n || ' 0 R');
    p_out('/Info ' || (n-1) || ' 0 R');
end p_puttrailer;

----------------------------------------------------------------------------------------
procedure p_endpage is
begin
    -- End of page contents
    state:=1;
end p_endpage;

----------------------------------------------------------------------------------------
procedure p_putpages is
   nb number := page;
   filter varchar2(200);
   annots bigtext;
   rect txt;
   -- l Array2dim;
   -- h number;
   kids txt;
   v_0 varchar2(255);
   v_1 varchar2(255);
   v_2 varchar2(255);
   v_3 varchar2(255);
   v_4 varchar2(255);
   v_0n number;
   v_1n number;
   v_2n number;
   v_3n number;
begin

   -- Replace number of pages
     if not empty(AliasNbPages) then
           for i in 1..nb loop
              pages(i) := str_replace_clob(AliasNbPages,nb,pages(i));
           end loop;
     end if;

     if DefOrientation = 'P' then
          wPt:=fwPt;
          hPt:=fhPt;
     else
          wPt:=fhPt;
          hPt:=fwPt;
   end if;

     if (b_compress) then
        filter := '/Filter /FlateDecode ';
     else
        filter := '';
     end if;

   for i in 1..nb loop
          p_newobj();
          p_out('<</Type /Page');
          p_out('/Parent 1 0 R');
          if(OrientationChanges.exists(i)) then
               p_out('/MediaBox [0 0 '||tochar(hPt)||' '||tochar(wPt)||']');
        end if;
          p_out('/Resources 2 0 R');

      if(PageLinks.exists(i)) then
               annots := '/Annots [';
         v_0 := PageLinks(i).zero;
         v_0n := tonumber(v_0);
         v_1 := PageLinks(i).un;
         v_1n := tonumber(v_1);
         v_2 := PageLinks(i).deux;
         v_2n := tonumber(v_2);
         v_3 := PageLinks(i).trois;
         v_3n := tonumber(v_3);
         v_4 := PageLinks(i).quatre;
               rect := tochar(v_0) || ' ' || tochar(v_1) || ' ' || tochar(v_0n + v_2n) || ' ' || tochar(v_1n - v_3n);
               annots := annots || '<</Type /Annot /Subtype /Link /Rect [' || rect || '] /Border [0 0 0] ';
         if is_string(PageLinks(i).quatre) then
                      annots := annots ||'/A <</S /URI /URI '||p_textstring(PageLinks(i).quatre) || '>>>>';
         end if;
               p_out(annots || ']');
      end if;

          p_out('/Contents ' || to_char(n+1) || ' 0 R>>');
          p_out('endobj');

        p_newobj();
         if (b_compress) then
            p_out('<<' || filter || '/Length ' || dbms_lob.getlength(flate_encode(c2b(pages(i)))) || '>>');
            p_putstream(flate_encode(c2b(pages(i))));
         else
            p_out('<<' || filter || '/Length ' || strlen(pages(i)) || '>>');
            p_putstream(pages(i));
         end if;
        
        
        p_out('endobj');
   end loop;

     offsets(1):=getPDFDocLength();
     p_out('1 0 obj');
     p_out('<</Type /Pages');
     kids := '/Kids [';
   
   for i in 0..nb-1 loop
        kids := kids || to_char(3+2*i) || ' 0 R ';
     end loop;
     
   p_out( kids || ']');
     p_out('/Count '|| nb);
     p_out('/MediaBox [0 0 '||tochar(wPt)||' '||tochar(hPt)||']');
     p_out('>>');
     p_out('endobj');
   
end p_putpages;

----------------------------------------------------------------------------------------
procedure p_enddoc is
o number;
begin
    p_putheader();
  p_putpages();
  p_putresources();
  -- Info
    p_newobj();
    p_out('<<');
    p_putinfo();
    p_out('>>');
    p_out('endobj');
    -- Catalog
    p_newobj();
    p_out('<<');
    p_putcatalog();
    p_out('>>');
    p_out('endobj');
    -- Cross-ref
    o := getPDFDocLength();
    p_out('xref');
    p_out('0 ' || (n+1));
    p_out('0000000000 65535 f ');
    for i in 1..n
    loop
      p_out(substr('0000000000', 1, 10 - length(offsets(i)) ) ||offsets(i) || ' 00000 n ');
    end loop;
    -- Trailer
    p_out('trailer');
    p_out('<<');
    p_puttrailer();
    p_out('>>');
    p_out('startxref');
    p_out(o);
    p_out('%%EOF');
    state := 3;
end p_enddoc;

----------------------------------------------------------------------------------------
procedure p_beginpage(orientation varchar2) is
Myorientation word := orientation;
begin
    page := page + 1;
    pages(page):=null;
    state:=2;
    x:=lMargin;
    y:=tMargin;
    FontFamily:='';
    -- Page orientation
    if(empty(Myorientation)) then
        Myorientation:=DefOrientation;
    else
        Myorientation := substr(Myorientation, 1, 1);
        Myorientation:=strtoupper(Myorientation);
        if(Myorientation!=DefOrientation) then
            OrientationChanges(page):=true;
        end if;
    end if;
    if(Myorientation!=CurOrientation) then
        -- Change orientation
        if(orientation='P') then
            wPt:=fwPt;
            hPt:=fhPt;
            w:=fw;
            h:=fh;
        else
            wPt:=fhPt;
            hPt:=fwPt;
            w:=fh;
            h:=fw;
        end if;
        pageBreakTrigger:=h-bMargin;
        CurOrientation:=Myorientation;
    end if;
end p_beginpage;

----------------------------------------------------------------------------------------
function p_dounderline(px number,py number,ptxt varchar2) return varchar2 is
up word := -100;
ut word := 50; 
w number := 0;
begin
    w:=str_len(ptxt)*fontsize/1000 + ws * substr_count(ptxt,' ');
    return tochar(px*k,2)||' '||tochar((h-(py-up/1000*fontsize))*k,2)||' '||tochar(w*k,2)||' '||tochar(-ut/1000*fontsizePt,2)||' re f';
end p_dounderline;


--------------------------------------------------------------------------------
-- Function to convert a binary unsigned integer
-- into a PLSQL number
--------------------------------------------------------------------------------
function p_freadint( p_data in varchar2 ) return number
is
    l_number number default 0;
    l_bytes  number default length(p_data);
    big_endian constant boolean default true;
begin
    if (big_endian)
    then
        for i in 1 .. l_bytes loop
            l_number := l_number +
                              ascii(substr(p_data,i,1)) *
                                           power(2,8*(i-1));
        end loop;
    else
        for i in 1 .. l_bytes loop
            l_number := l_number +
                         ascii(substr(p_data,l_bytes-i+1,1)) *
                         power(2,8*(i-1));
        end loop;
    end if;

    return l_number;
end p_freadint;



function raw_to_num( p_value in raw) return number is
begin                                                -- note: FFFFFFFF => -1
  return utl_raw.cast_to_binary_integer( p_value );
end;

function raw2num( p_value raw )
  return number
  is
  begin
    return to_number( rawtohex( p_value ), 'XXXXXXXX' );
  end;
--
  function raw2num( p_value raw, p_pos pls_integer, p_len pls_integer )
  return pls_integer
  is
  begin
    return to_number( rawtohex( utl_raw.substr( p_value, p_pos, p_len ) ), 'XXXXXXXX' );
  end;
--
--


function adler32( p_src in blob )    return varchar2 is
    s1 pls_integer := 1;
    s2 pls_integer := 0;
begin
  for i in 1 .. dbms_lob.getlength( p_src )  loop
    s1 := mod( s1  + utl_raw.cast_to_binary_integer( dbms_lob.substr( p_src, 1 , i  ) ) , 65521  );
    s2 := mod( s2 + s1  , 65521 );
  end loop;
  return to_char( s2 , 'fm0XXX' ) || to_char( s1 , 'fm0XXX'  );
end;

function blobChunk(b blob, amt number, offset number ) return blob is

  t  blob;

begin

  dbms_lob.createtemporary( t, true );
  dbms_lob.copy( t, b,amt, 1, offset );
  return t;
  
end;
  function pdf_string( p_txt in blob )
  return blob
  is
    t_rv blob;
    t_ind integer;
    type tp_tab_raw is table of raw(1);
    tab_raw tp_tab_raw
      := tp_tab_raw( utl_raw.cast_to_raw( '\' )
                   , utl_raw.cast_to_raw( '(' )
                   , utl_raw.cast_to_raw( ')' )
                   );
  begin
    t_rv := p_txt;
    for i in tab_raw.first .. tab_raw.last
    loop
      t_ind := -1;
      loop
        t_ind := dbms_lob.instr( t_rv
                               , tab_raw( i )
                               , t_ind + 2
                               );
        exit when t_ind <= 0;
        dbms_lob.copy( t_rv
                     , t_rv
                     , dbms_lob.lobmaxsize
                     , t_ind + 1
                     , t_ind
                     );
        dbms_lob.copy( t_rv
                     , utl_raw.cast_to_raw( '\' )
                     , 1
                     , t_ind
                     , 1
                     );
      end loop;
    end loop;
    return t_rv;
  end;
--
   
  function txt2raw( p_txt varchar2 )
  return varchar2
  is
    t_rv raw(32767);
    w_return varchar2(32767);
    t_unicode pls_integer;
  begin
    if g_current_font is null
    then
      set_font( 'helvetica','N',9 );
    end if;
    if g_fonts( g_current_font ).cid
    then
      for i in 1 .. length( p_txt )
      loop
        t_unicode := utl_raw.cast_to_binary_integer( utl_raw.convert( utl_raw.cast_to_raw( substr( p_txt, i, 1 ) )
                                                                    , 'AMERICAN_AMERICA.AL16UTF16'
                                                                    , sys_context( 'userenv', 'LANGUAGE' )  -- ???? font characterset ?????
                                                                    )
                                                 );
        if g_fonts( g_current_font ).flags = 4 -- a symbolic font
        then
-- assume code 32, space maps to the first code from the font
          t_unicode := g_fonts( g_current_font ).code2glyph.first + t_unicode - 32;
        end if;
        if g_fonts( g_current_font ).code2glyph.exists( t_unicode )
        then
          g_fonts( g_current_font ).used_chars( g_fonts( g_current_font ).code2glyph( t_unicode ) ) := 0;
          t_rv := utl_raw.concat( t_rv
                                , utl_raw.cast_to_raw( to_char( g_fonts( g_current_font ).code2glyph( t_unicode ), 'FM0XXX' ) )
                                );
        else
          t_rv := utl_raw.concat( t_rv, utl_raw.cast_to_raw( '0000' ) );
        end if;
      end loop;
      t_rv := utl_raw.concat( utl_raw.cast_to_raw( '<' )
                            , t_rv
                            , utl_raw.cast_to_raw( '>' )
                            );
      
    else
      t_rv := utl_raw.convert( utl_raw.cast_to_raw( p_txt )
                             , g_fonts( g_current_font ).charset
                             , sys_context( 'userenv', 'LANGUAGE' )
                             );
      for i in 1 .. utl_raw.length( t_rv )
      loop
        g_fonts( g_current_font ).used_chars( raw2num( t_rv, i, 1 ) ) := 0;
      end loop;
      t_rv := utl_raw.concat( utl_raw.cast_to_raw( '(' )
                            , pdf_string( t_rv )
                            , utl_raw.cast_to_raw( ')' )
                            );
      
    end if;
    w_return := utl_raw.cast_to_varchar2(t_rv);
    return w_return;
  end;
  

function parse_png( p_img_blob in blob )  return tp_img  is
  t_img tp_img;
  buf raw( 32767 );
  len integer;
  ind integer;
  color_type pls_integer;
  tBlob blob;

begin


  if rawtohex( dbms_lob.substr( p_img_blob, 8, 1) ) != '89504E470D0A1A0A' then                                            -- not the right signature
   raise_application_error(-20300,'Unknown image type.');
  end if;
  dbms_lob.createtemporary( t_img.pixels, true );
  ind := 9;


  loop

    len := raw_to_num( dbms_lob.substr( p_img_blob , 4, ind ) );                            -- length
    exit when len is null or ind > dbms_lob.getlength( p_img_blob );

    case utl_raw.cast_to_varchar2(dbms_lob.substr( p_img_blob, 4, ind + 4 ) )          -- Chunk type
      when 'IHDR' then
        t_img.width            := raw_to_num( dbms_lob.substr( p_img_blob, 4, ind + 8));
        t_img.height         := raw_to_num( dbms_lob.substr( p_img_blob, 4, ind + 12));
        t_img.color_res := raw_to_num( dbms_lob.substr( p_img_blob, 1, ind + 16));
        color_type             := raw_to_num( dbms_lob.substr( p_img_blob, 1, ind + 17));
        t_img.greyscale := color_type in( 0, 4 );
        
        if color_type = 6 then
          raise_application_error(-20300,'Alpha channel not supported.');
        end if;
        
      when 'PLTE'  then
        t_img.color_tab := dbms_lob.substr( p_img_blob, len, ind + 8);
      when 'IDAT' then
        dbms_lob.append( t_img.pixels,  blobChunk(p_img_blob, (len), (ind+8))  );
      when 'IEND' then
        exit;
      else
        null;
    end case;

    ind := ind + 4 + 4 + len + 4;  -- Length + Chunk type + Chunk data + CRC 
  end loop;

  t_img.type := 'png';
  t_img.nr_colors :=  case color_type
                        when 0 then 1
                        when 2 then 3
                        when 3 then 1
                        when 4 then 2
                        else 4
                      end;
  return t_img;
end;

--
function parse_jpg( p_img_blob in blob  )return tp_img  is
  buf raw( 4 );
    t_img tp_img;
  t_ind integer;
begin

  if ( dbms_lob.substr( p_img_blob, 2, 1 ) != hextoraw( 'FFD8' )  or 
           dbms_lob.substr( p_img_blob, 2, dbms_lob.getlength( p_img_blob ) - 1 ) != hextoraw( 'FFD9' ) ) then  
   raise_application_error(-20300,'Unknown image type.');
  end if;
  
  t_img.pixels := p_img_blob;
  t_img.type := 'jpg';

  if dbms_lob.substr( t_img.pixels, 2, 3 ) in  ( hextoraw( 'FFE0' ), hextoraw( 'FFE1' ) ) then
    t_img.color_res := 8;
    t_img.height := 1;
    t_img.width := 1;
    t_ind := 3;
    t_ind := t_ind + 2 + raw_to_num( dbms_lob.substr( t_img.pixels, 2, t_ind + 2) );

    loop
      buf := dbms_lob.substr( t_img.pixels, 2, t_ind );
      exit when buf = hextoraw( 'FFDA' );               -- SOS Start of Scan
      exit when buf = hextoraw( 'FFD9' );                -- EOI End Of Image
      exit when substr( rawtohex( buf ), 1, 2 ) != 'FF';

      if rawtohex( buf ) in
           ( 'FFD0'                                                  -- RSTn
           , 'FFD1', 'FFD2', 'FFD3', 'FFD4', 'FFD5', 'FFD6', 'FFD7'
           , 'FF01'                                                   -- TEM
           )
      then
        t_ind := t_ind + 2;
      else
        if buf = hextoraw( 'FFC0' )       -- SOF0 (Start Of Frame 0) marker
        then
          t_img.color_res    := raw_to_num( dbms_lob.substr( t_img.pixels, 1, t_ind + 4));
          t_img.height        := raw_to_num( dbms_lob.substr( t_img.pixels, 2, t_ind + 5));
          t_img.width         := raw_to_num( dbms_lob.substr( t_img.pixels, 2, t_ind + 7));
        end if;
        t_ind := t_ind + 2 + raw_to_num( dbms_lob.substr( t_img.pixels, 2, t_ind + 2));
      end if;
    end loop;
  end if;

  return t_img;
end;


function parse_img(p_blob  blob, p_adler32  varchar2 := null)  return tp_img is
  img tp_img;
begin


  if rawtohex( dbms_lob.substr( p_blob, 8, 1)) = '89504E470D0A1A0A'  then
    img     := parse_png( p_blob );
  else
    img     := parse_jpg( p_blob );
  end if;

  if img.width is not null then
    img.adler32 := p_adler32;
  end if;

  return img;
end;




/*******************************************************************************
*                                                                              *
*                               Public methods                                 *
*                                                                              *
********************************************************************************/

----------------------------------------------------------------------------------------
-- Methods added to FPDF primary class
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
-- SetDash Ecrire en pointill
----------------------------------------------------------------------------------------
procedure SetDash(pblack number default 0, pwhite number default 0) is
  s txt;
begin
    if(pblack != 0 or pwhite != 0) then
        s := '['||tochar(pblack*k, 3)||' '||tochar( pwhite*k, 3)||'] 0 d';
    else
        s := '[] 0 d';
    end if;
    p_out(s);
end SetDash;




----------------------------------------------------------------------------------------
-- Methods from FPDF primary class
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
procedure xError(pmsg varchar2) is
begin

    if gb_mode_debug then
      print('<pre>');
    htpc(clobfromblob(pdfBoc));
    
      --for i in pdfDoc.first..pdfDoc.last loop
      --  if i is not null then
      --    print(replace(replace(pdfDoc(i),'>',';'),'<',';'));
        --end if;
      --end loop;
      print('</pre>');
    return;
    end if;
 
    -- Fatal error
    raise_application_error(-20100,'<B>PL_FPDF error: </B>'|| pmsg);
end xError;

----------------------------------------------------------------------------------------
function GetCurrentFontSize return number is
begin
    -- Get fontsizePt
    return fontsizePt;
end GetCurrentFontSize;

----------------------------------------------------------------------------------------
function GetCurrentFontStyle return varchar2 is
begin
    -- Get fontStyle
    return fontStyle;
end GetCurrentFontStyle;

----------------------------------------------------------------------------------------
function GetCurrentFontFamily return varchar2 is
begin
    -- Get fontStyle
    return FontFamily;
end GetCurrentFontFamily;


----------------------------------------------------------------------------------------
procedure Ln(h number default null) is
begin
    -- Line feed; default value is last cell height
    x :=lMargin;
    if h is null  or  is_string(h) then
        y:= y + lasth;
    else
        y:= y + h;
  end if;
end Ln;

----------------------------------------------------------------------------------------
function GetXpx return number is
begin
  -- Get x in pixels
  return x*k;
end;

function GetX return number is
begin
    -- Get x position
    return x;
end GetX;

----------------------------------------------------------------------------------------
procedure SetX(px number) is
begin
    -- Set x position
    if(px>=0) then
        x:=px;
    else
        x:=w+px;
    end if;
end SetX;


function GetYpx return number is
begin
  -- Get y in pixels
  return y*k;
end ;


function GetY return number is
begin
    -- Get y position
    return y;
end GetY;


procedure SetY(py number) is
begin
    -- Set y position and reset x
    x:=lMargin;
    if(py>=0) then
        y:=py;
    else
        y:=h+py;
    end if;
end SetY;


procedure SetXY(x number,y number) is
begin
    -- Set x and y positions
    SetY(y);
    SetX(x);
end SetXY;


function GetPageHeight return number is
begin
  return h;
end;


----------------------------------------------------------------------------------------
-- SetHeaderProc : setting header Callback
----------------------------------------------------------------------------------------
procedure SetHeaderProc(headerprocname in varchar2, paramTable tv4000a) is
begin
   MyHeader_Proc := headerprocname;
   MyHeader_ProcParam := paramTable;
end;

----------------------------------------------------------------------------------------
-- SetFooterProc : setting footer Callback
----------------------------------------------------------------------------------------
procedure SetFooterProc(footerprocname in varchar2, paramTable tv4000a) is
begin
   MyFooter_Proc := footerprocname;
   MyFooter_ProcParam := paramTable;
end;

----------------------------------------------------------------------------------------
procedure SetMargins(left number,top number ,right number default -1) is
myright margin := right;
begin
    -- Set left, top and right margins
    lMargin:=left;
    tMargin:=top;
    if(myright=-1) then
        myright:=left;
    end if;
    rMargin:=myright;
end SetMargins;

----------------------------------------------------------------------------------------
procedure SetLeftMargin( pMargin number) is
begin
    -- Set left margin
    lMargin:=pMargin;
    if(page > 0 and  x < pMargin) then
        x:= pMargin;
    end if;
end SetLeftMargin;

----------------------------------------------------------------------------------------
procedure SetTopMargin(pMargin number) is
begin
    -- Set top margin
    tMargin := pMargin;
end SetTopMargin;

----------------------------------------------------------------------------------------
procedure SetRightMargin(pMargin number) is
begin
    -- Set right margin
    rMargin := pMargin;
end SetRightMargin;

----------------------------------------------------------------------------------------
procedure SetAutoPageBreak(pauto boolean, pMargin number default 13) is
begin
    -- Set auto page break mode and triggering margin
    AutoPageBreak := pauto;
    bMargin := pMargin;
    pageBreakTrigger:=h-pMargin;
end SetAutoPageBreak;

----------------------------------------------------------------------------------------
procedure SetDisplayMode(zoom varchar2,layout varchar2 default 'continuous') is
begin
    -- Set display mode in viewer
    if(zoom in ('fullpage', 'fullwidth', 'real', 'default') or not is_string(zoom)) then
        ZoomMode:= zoom;
    else
            raise_application_error(-20100,'Incorrect zoom display mode: ' || zoom);
    end if;
    if(layout in ('single', 'continuous', 'two', 'default')) then
        LayoutMode := layout;
    else
        raise_application_error(-20100,'Incorrect layout display mode: ' || layout);
    end if;
end SetDisplayMode;

----------------------------------------------------------------------------------------
procedure SetCompression(p_compress boolean default true) is
begin
    -- Set page compression
    if( p_compress ) then
        b_compress:=p_compress;
    else
        b_compress:=false;
    end if;
end SetCompression;

----------------------------------------------------------------------------------------
procedure SetTitle(ptitle varchar2) is
begin
    -- Title of document
    title:=ptitle;
end SetTitle;

----------------------------------------------------------------------------------------
procedure SetSubject(psubject varchar2) is
begin
    -- Subject of document
    subject:= psubject;
end SetSubject;

----------------------------------------------------------------------------------------
procedure SetAuthor(pauthor varchar2) is
begin
    -- Author of document
    author:=pauthor;
end SetAuthor;

----------------------------------------------------------------------------------------
procedure SetKeywords(pkeywords varchar2) is
begin
    -- Keywords of document
    keywords:=pkeywords;
end SetKeywords;

----------------------------------------------------------------------------------------
procedure SetCreator(pcreator varchar2) is
begin
    -- Creator of document
    creator:=pcreator;
end SetCreator;

----------------------------------------------------------------------------------------
procedure SetAliasNbPages(palias varchar2 default '{nb}') is
begin
    -- Define an alias for total number of pages
    AliasNbPages:=palias;
end SetAliasNbPages;


----------------------------------------------------------------------------------------
-- buildPlsqlStatment : building the pl/lsq stmt for header or Footer hooked custom proc
--                      Binding parameters and values.
----------------------------------------------------------------------------------------
function buildPlsqlStatment(callbackProc varchar2, tParam tv4000a default noParam) return varchar2 is
    plsqStmt bigtext;
    paramName word;
begin
    if (tParam.first is not null) then
        -- retrieving values of parameters to build plssql statment.
        plsqStmt := 'Begin '||callbackProc||'(';
        paramName := tParam.first;
        while (paramName is not null) loop
            if (paramName != tParam.first) then
                plsqStmt := plsqStmt || ', ';
            end if;
            plsqStmt := plsqStmt || paramName ||'=>'''||
                                    replace(tParam(paramName), '''', '''''')||'''';
            paramName := tParam.next(paramName);
        end loop;
        plsqStmt := plsqStmt||'); end;';
    else
        plsqStmt := 'Begin '||callbackProc||'; end;';
    end if;
    return plsqStmt;
end buildPlsqlStatment;

----------------------------------------------------------------------------------------
-- Header : Procedure that hook the callback procedure for the repetitive header on each page;
----------------------------------------------------------------------------------------
procedure Header is
    plsqStmt bigtext;
begin
    -- MyHeader_Proc defined in Declaration
    if (not empty(MyHeader_Proc)) then
        -- building plsql stmt.
        plsqStmt := buildPlsqlStatment(MyHeader_Proc, MyHeader_ProcParam);
        -- Executing callback.
        execute immediate plsqStmt;
    end if;
end Header;

----------------------------------------------------------------------------------------
-- Footer : Procedure that hook the callback procedure for the repetitive footer on each page;
----------------------------------------------------------------------------------------
procedure Footer is
    plsqStmt bigtext;
begin
    -- MyFooter_Proc defined in Declaration
    if (not empty(MyFooter_Proc)) then
        -- building plsql stmt.
        plsqStmt := buildPlsqlStatment(MyFooter_Proc, MyFooter_ProcParam);
        -- Executing callback.
       execute immediate plsqStmt;
    end if;
end Footer;

----------------------------------------------------------------------------------------
function PageNo return number is
begin
    -- Get current page number
    return page;
end PageNo;

----------------------------------------------------------------------------------------
procedure SetDrawColor(r number,g number default -1,b number default -1) is
begin
    -- Set color for all stroking operations
    if((r=0 and g=0 and b=0) or g=-1)  then
        DrawColor:=tochar(r/255,3)||' G';
    else
        DrawColor:=tochar(r/255,3) || ' ' || tochar(g/255,3) || ' ' || tochar(b/255,3) || ' RG';
    end if;
    if(page>0) then
        p_out(DrawColor);
    end if;
end SetDrawColor;

----------------------------------------------------------------------------------------
procedure SetFillColor (r number,g number default -1,b number default -1) is
begin
    -- Set color for all filling operations
    if((r=0 and g=0 and b=0) or g=-1) then
        FillColor:=tochar(r/255,3) || ' g';
    else
        FillColor:=tochar(r/255,3) ||' '|| tochar(g/255,3) ||' '|| tochar(b/255,3) || ' rg';
    end if;
    if (FillColor!=TextColor) then
      ColorFlag:=true;
    else
      ColorFlag:=false;
    end if;
    if(page>0) then
        p_out(FillColor);
    end if;
end SetFillColor;

----------------------------------------------------------------------------------------
procedure SetTextColor (r number,g number default -1,b number default -1) is
begin
    -- Set color for text
    if((r=0 and g=0 and b=0) or g=-1) then
        TextColor:=tochar(r/255,3) || ' g';
    else
        TextColor:=tochar(r/255,3) ||' '|| tochar(g/255,3) ||' '|| tochar(b/255,3) || ' rg';
    end if;
    if (FillColor!=TextColor) then
      ColorFlag:=true;
    else
      ColorFlag:=false;
    end if;
end SetTextColor;

----------------------------------------------------------------------------------------
procedure SetLineWidth(width number) is
begin
    -- Set line width
    LineWidth:=width;
    if(page>0) then
        p_out(tochar(width*k,2) ||' w');
    end if;
end SetLineWidth;

----------------------------------------------------------------------------------------
procedure Line(x1 number,y1 number,x2 number,y2 number) is
begin
    -- Draw a line
    p_out( tochar(x1*k,2) ||
           ' ' || tochar((h-y1)*k,2) ||
           ' m ' || tochar(x2*k,2) ||
           ' ' || tochar((h-y2)*k,2) || ' l S');
end Line;

----------------------------------------------------------------------------------------
procedure Rect(px number,py number,pw number,ph number,pstyle varchar2 default '') is
op word;
begin
    -- Draw a rectangle
    if(pstyle='F') then
        op:='f';
    elsif(pstyle='FD' or pstyle='DF') then
        op:='B';
    else
        op:='S';
    end if;
    p_out(tochar(px*k,2) || ' ' || tochar((h-py)*k,2) || ' ' || tochar(pw*k,2) || ' ' || tochar(-ph*k,2) || ' re ' || op);
end Rect;


----------------------------------------------------------------------------------------
function AddLink return number is
nb_link number := links.count + 1;
begin
    -- Create a new internal link
    links(nb_link).zero := 0;
    links(nb_link).un := 0;
    return nb_link;
end AddLink;

----------------------------------------------------------------------------------------
procedure SetLink(plink number,py number default 0,ppage number default -1) is
mypy number := py;
myppage number := ppage;
begin
    -- Set destination of internal link
    if(mypy=-1) then
        mypy:=y;
    end if;
    if(myppage=-1) then
        myppage:=page;
    end if;
    links(plink).zero:=myppage;
    links(plink).un:=mypy;
end SetLink;

----------------------------------------------------------------------------------------
procedure Link(px number,py number,pw number,ph number,plink varchar2) is
  v_last_plink integer;
  v_ntoextend integer;
  v_rec rec5;
begin
    -- Put a link on the page
  -- Init PageLinks, if not exists
  begin
     v_last_plink := PageLinks.count;
  exception
     when others then
        PageLinks := linksArray(v_rec);
  end;
  -- extend, so PageLinks(page) exists
  v_last_plink := PageLinks.last;
  v_ntoextend := page-v_last_plink;
  if v_ntoextend > 0 then
     PageLinks.extend(v_ntoextend);
  end if;
  -- set values
    PageLinks(page).zero:=px*k;
    PageLinks(page).un:=hPt-py*k;
    PageLinks(page).deux:=pw*k;
    PageLinks(page).trois:=ph*k;
    PageLinks(page).quatre:=plink;
end Link;


----------------------------------------------------------------------------------------
procedure Text(px number,py number,ptxt varchar2, p_degrees_rotation number default null) is
s varchar2(2000);
    c_pi constant number := 3.14159265358979323846264338327950288419716939937510;
    t_tmp varchar2(32767);
    t_sin number;
    t_cos number;
begin

    t_tmp := tochar(px*k,2) || ' ' || tochar((h-py)*k,2);
    if p_degrees_rotation is null
    then
      t_tmp := t_tmp || ' Td ';
    else
      t_sin := sin( p_degrees_rotation / 180 * c_pi );
      t_cos := cos( p_degrees_rotation / 180 * c_pi );
      t_tmp := tochar( t_cos, 5 ) || ' ' || t_tmp;
      t_tmp := tochar( - t_sin, 5 ) || ' ' || t_tmp;
      t_tmp := tochar( t_sin, 5 ) || ' ' || t_tmp;
      t_tmp := tochar( t_cos, 5 ) || ' ' || t_tmp;
      t_tmp := t_tmp || ' Tm ';
    end if;
    -- Output a string
    s:='BT '|| t_tmp ||txt2raw(ptxt)||' Tj ET';
    if(underline and ptxt is not null) then
        s := s || ' ' || p_dounderline(px,py,ptxt);
    end if;
    if(ColorFlag) then
        s := 'q '|| TextColor ||' ' || s || ' Q';
    end if;
    p_out(s);
end Text;

----------------------------------------------------------------------------------------
function AcceptPageBreak return boolean is
begin
    -- Accept automatic page break or not
    return AutoPageBreak;
end AcceptPageBreak;


----------------------------------------------------------------------------------------
procedure OpenPDF is
begin
    -- Begin document
    state:=1;
    DBMS_LOB.CREATETEMPORARY(lob_loc=>pdfBoc, cache=>TRUE, dur=>dbms_lob.SESSION);
   
    g_fonts.delete;
    g_used_fonts.delete;
    g_current_font := null;  

    init_core_fonts;    
end OpenPDF;

----------------------------------------------------------------------------------------
procedure ClosePDF is
begin
   

    -- Terminate document
    if(state=3) then
      return;
    end if;

    if(page=0) then
        AddPage();
    end if;
  -- Page footer
    InFooter:=true;
    Footer();
    InFooter:=false;
    -- Close page
    p_endpage();
    -- Close document

    p_enddoc();

end ClosePDF;

----------------------------------------------------------------------------------------
procedure AddPage(orientation varchar2 default '') is

  myFamily  txt;
  myStyle   txt;
  mySize    number     := fontsizePt;
  lw        phrase     := LineWidth;
  dc        phrase     := DrawColor;
  fc        phrase     := FillColor;
  tc        phrase     := TextColor;
  cf        flag       := ColorFlag;

begin
    -- Start a new page
    if(state=0) then
        OpenPDF();
    end if;
    myFamily:= FontFamily;
    if (underline) then
       myStyle := FontStyle || 'U';
    end if;
    if(page>0) then
        -- Page footer
        InFooter:=true;
        Footer();
        InFooter:=false;
        -- Close page
        p_endpage();
    end if;
    -- Start new page
    p_beginpage(orientation);
    -- Set line cap style to square
    p_out('2 J');
    -- Set line width
    LineWidth:=lw;
    p_out(tochar(lw*k)||' w');
    -- Set font
    if(myFamily is not null) then
        Set_Font(myFamily,myStyle,mySize);
    end if;
    -- Set colors
    DrawColor:=dc;
    if(dc!='0 G') then
        p_out(dc);
    end if;
    FillColor:=fc;

    if(fc!='0 g') then
        p_out(fc);
    end if;

    TextColor:= tc;
    ColorFlag:= cf;
    -- Page header
    header();
  
  thdraw();
  
    -- Restore line width
    if(LineWidth!=lw) then
        LineWidth:=lw;
        p_out(tochar(lw*k)||' w');
    end if;
    -- Restore font

    if myFamily is null then
        Set_Font(myFamily,myStyle,mySize);
    end if;
    -- Restore colors
    if(DrawColor!=dc) then
        DrawColor:=dc;
        p_out(dc);
    end if;
    if(FillColor!=fc) then
        FillColor:=fc;
        p_out(fc);
    end if;
    TextColor:=tc;
    ColorFlag:=cf;
end AddPage;

----------------------------------------------------------------------------------------
procedure pdf (
                    orientation varchar2 default 'P',
                     unit        varchar2 default 'mm',
                     format      varchar2 default 'A4'
                ) is
                
   myorientation word     := orientation;
   myformat word                 := format;
   mymargin margin;
   
begin
    
    -- Initialization of properties
    page:=0;
    n:=2;
    -- Open the final structure for the PDF document.
    --pdfDoc(1) := null;
    state:=0;
    InFooter:=false;
    lasth:=0;
    --FontFamily:='';
    FontFamily:='helvetica';
    fontstyle:='';
    fontsizePt:=12;
    underline:=false;
    DrawColor:='0 G';
    FillColor:='0 g';
    TextColor:='0 g';
    ColorFlag:=false;
    ws:=0;

    -- Scale factor
    if(unit='pt') then
        k:=1;
    elsif(unit='mm') then
        k:=72/25.4;
    elsif(unit='cm') then
        k:=72/2.54;
    elsif(unit='in') then
        k:=72;
    else
            raise_application_error(-20100,'Incorrect unit: ' || unit);
    end if;

    -- Others added properties
    Linespacing := fontsizePt / k;    -- minimum line spacing in multicell

    -- Page format
    if(is_string(myformat)) then
    
      myformat:=strtolower(myformat);
        if(myformat='a3') then
            formatArray.largeur := 841.89;
            formatArray.hauteur := 1190.55;
        elsif(myformat='a4') then
            formatArray.largeur := 595.28;
            formatArray.hauteur := 841.89;
        elsif(myformat='a5') then
            formatArray.largeur := 420.94;
            formatArray.hauteur := 595.28;
    elsif(myformat='thermal') then
      formatArray.largeur := 576;  --225;
      formatArray.hauteur := 300;  --225; 
      elsif(myformat='letter') then
            formatArray.largeur := 612;
            formatArray.hauteur := 792;
        elsif(myformat='legal') then
            formatArray.largeur := 612;
            formatArray.hauteur := 1008;
        else
                raise_application_error(-20100,'Unknown page format: '|| myformat);
        end if;
    
        fwPt:=formatArray.largeur;
        fhPt:=formatArray.hauteur;
    
  else
    
      fwPt:=formatArray.largeur*k;
        fhPt:=formatArray.hauteur*k;
    
  end if;
    
  fw:=fwPt/k;
    fh:=fhPt/k;
    -- Page orientation
    myorientation:=strtolower(myorientation);
    if(myorientation='p' or  myorientation='portrait') then
        DefOrientation:='P';
        wPt:=fwPt;
        hPt:=fhPt;
    elsif(myorientation='l' or myorientation='landscape') then
        DefOrientation:='L';
        wPt:=fhPt;
        hPt:=fwPt;
    else
            raise_application_error(-20100,'Incorrect orientation: ' || myorientation);
    end if;
    CurOrientation:=DefOrientation;
    w:=wPt/k;
    h:=hPt/k;
    -- Page margins (1 cm)
    mymargin:=28.35/k;
    SetMargins(mymargin,mymargin);
    -- Interior cell margin (1 mm)
    cMargin:=mymargin/10;
    -- Line width (0.2 mm)
    LineWidth:=.567/k;
    -- Automatic page break 
    SetAutoPageBreak(true,2*mymargin);
    -- Full width display mode
    SetDisplayMode('fullwidth');
    -- Disable compression
    --SetCompression(false);
    -- Set default PDF version number
    PDFVersion:='1.3';
end pdf;

----------------------------------------------------------------------------------------


procedure fontSave is
begin

  fontFamilySave := FontFamily;
  fontStyleSave  := FontStyle;
  fontSizePtSave := FontsizePt;

end;

---------- as_pdf 3

--
  function to_short( p_val raw, p_factor number := 1 )
  return number
  is
    t_rv number;
  begin
    t_rv := to_number( rawtohex( p_val ), 'XXXXXXXXXX' );
    if t_rv > 32767
    then
      t_rv := t_rv - 65536;
    end if;
    return t_rv * p_factor;
  end;

  function blob2num( p_blob blob, p_len integer, p_pos integer )
  return number
  is
  begin
    return to_number( rawtohex( dbms_lob.substr( p_blob, p_len, p_pos ) ), 'xxxxxxxx' );
  end;
  
  function num2raw( p_value number )
  return raw
  is
  begin
    return hextoraw( to_char( p_value, 'FM0XXXXXXX' ) );
  end;
--

  procedure raw2pdfdoc( p_raw blob )
  is
  begin
    dbms_lob.append( pdfBoc, p_raw );
  end;
  
  procedure txt2pdfdoc( p_txt varchar2 )
  is
  begin
    raw2pdfdoc( utl_raw.cast_to_raw( p_txt || c_nl ) );
  end;  
  
  /*
  function file2blob( p_dir varchar2, p_file_name varchar2 )
  return blob
  is
    t_raw raw(32767);
    t_blob blob;
    fh utl_file.file_type;
  begin
    fh := utl_file.fopen( p_dir, p_file_name, 'rb' );
    dbms_lob.createtemporary( t_blob, true );
    loop
      begin
        utl_file.get_raw( fh, t_raw );
        dbms_lob.append( t_blob, t_raw );
      exception
        when no_data_found
        then
          exit;
      end;
    end loop;
    utl_file.fclose( fh );
    return t_blob;
  exception
    when others
    then
      if utl_file.is_open( fh )
      then
        utl_file.fclose( fh );
      end if;
      raise;
  end;
  */
  procedure init_core_fonts
  is
    function uncompress_withs( p_compressed_tab varchar2 )
    return tp_pls_tab
    is
      t_rv tp_pls_tab;
      t_tmp raw(32767);
    begin
      if p_compressed_tab is not null
      then
        t_tmp := utl_compress.lz_uncompress

          ( utl_encode.base64_decode( utl_raw.cast_to_raw( p_compressed_tab ) ) );
        for i in 0 .. 255
        loop
          t_rv( i ) := to_number( utl_raw.substr( t_tmp, i * 4 + 1, 4 ), '0xxxxxxx' );
        end loop;
      end if;
      return t_rv;
    end;
    
  procedure txt2pdfdoc( p_txt varchar2 )
  is
  begin
    raw2pdfdoc( utl_raw.cast_to_raw( p_txt || c_nl ) );
  end; 
   
  
  
--
    procedure init_core_font
      ( p_ind pls_integer
      , p_family varchar2
      , p_style varchar2
      , p_name varchar2
      , p_compressed_tab varchar2
      )
    is
    begin
      g_fonts( p_ind ).family := p_family;
      g_fonts( p_ind ).style := p_style;
      g_fonts( p_ind ).name := p_name;
      g_fonts( p_ind ).fontname := p_name;
      g_fonts( p_ind ).standard := true;
      g_fonts( p_ind ).encoding := 'EE8MSWIN1250';--WE8MSWIN1252
      g_fonts( p_ind ).charset := sys_context( 'userenv', 'LANGUAGE' );
      g_fonts( p_ind ).charset := substr( g_fonts( p_ind ).charset
                                        , 1
                                        , instr( g_fonts( p_ind ).charset, '.' )
                                        ) || g_fonts( p_ind ).encoding;
      g_fonts( p_ind ).char_width_tab := uncompress_withs( p_compressed_tab );
    end;
  begin
    init_core_font( 1, 'helvetica', 'N', 'Helvetica'
      ,  'H4sIAAAAAAAAC81Tuw3CMBC94FQMgMQOLAGVGzNCGtc0dAxAT+8lsgE7RKJFomOA'
      || 'SLT4frHjBEFJ8XSX87372C8A1Qr+Ax5gsWGYU7QBAK4x7gTnGLOS6xJPOd8w5NsM'
      || '2OvFvQidAP04j1nyN3F7iSNny3E6DylPeeqbNqvti31vMpfLZuzH86oPdwaeo6X+'
      || '5X6Oz5VHtTqJKfYRNVu6y0ZyG66rdcxzXJe+Q/KJ59kql+bTt5K6lKucXvxWeHKf'
      || '+p6Tfersfh7RHuXMZjHsdUkxBeWtM60gDjLTLoHeKsyDdu6m8VK3qhnUQAmca9BG'
      || 'Dq3nP+sV/4FcD6WOf9K/ne+hdav+DTuNLeYABAAA' );
--
    init_core_font( 2, 'helvetica', 'I', 'Helvetica-Oblique'
      ,  'H4sIAAAAAAAAC81Tuw3CMBC94FQMgMQOLAGVGzNCGtc0dAxAT+8lsgE7RKJFomOA'
      || 'SLT4frHjBEFJ8XSX87372C8A1Qr+Ax5gsWGYU7QBAK4x7gTnGLOS6xJPOd8w5NsM'
      || '2OvFvQidAP04j1nyN3F7iSNny3E6DylPeeqbNqvti31vMpfLZuzH86oPdwaeo6X+'
      || '5X6Oz5VHtTqJKfYRNVu6y0ZyG66rdcxzXJe+Q/KJ59kql+bTt5K6lKucXvxWeHKf'
      || '+p6Tfersfh7RHuXMZjHsdUkxBeWtM60gDjLTLoHeKsyDdu6m8VK3qhnUQAmca9BG'
      || 'Dq3nP+sV/4FcD6WOf9K/ne+hdav+DTuNLeYABAAA' );
--
    init_core_font( 3, 'helvetica', 'B', 'Helvetica-Bold'
      ,  'H4sIAAAAAAAAC8VSsRHCMAx0SJcBcgyRJaBKkxXSqKahYwB6+iyRTbhLSUdHRZUB'
      || 'sOWXLF8SKCn+ZL/0kizZuaJ2/0fn8XBu10SUF28n59wbvoCr51oTD61ofkHyhBwK'
      || '8rXusVaGAb4q3rXOBP4Qz+wfUpzo5FyO4MBr39IH+uLclFvmCTrz1mB5PpSD52N1'
      || 'DfqS988xptibWfbw9Sa/jytf+dz4PqQz6wi63uxxBpCXY7uUj88jNDNy1mYGdl97'
      || '856nt2f4WsOFed4SpzumNCvlT+jpmKC7WgH3PJn9DaZfA42vlgh96d+wkHy0/V95'
      || 'xyv8oj59QbvBN2I/iAuqEAAEAAA=' );
--
    init_core_font( 4, 'helvetica', 'BI', 'Helvetica-BoldOblique'
      ,  'H4sIAAAAAAAAC8VSsRHCMAx0SJcBcgyRJaBKkxXSqKahYwB6+iyRTbhLSUdHRZUB'
      || 'sOWXLF8SKCn+ZL/0kizZuaJ2/0fn8XBu10SUF28n59wbvoCr51oTD61ofkHyhBwK'
      || '8rXusVaGAb4q3rXOBP4Qz+wfUpzo5FyO4MBr39IH+uLclFvmCTrz1mB5PpSD52N1'
      || 'DfqS988xptibWfbw9Sa/jytf+dz4PqQz6wi63uxxBpCXY7uUj88jNDNy1mYGdl97'
      || '856nt2f4WsOFed4SpzumNCvlT+jpmKC7WgH3PJn9DaZfA42vlgh96d+wkHy0/V95'
      || 'xyv8oj59QbvBN2I/iAuqEAAEAAA=' );
--
    init_core_font( 5, 'times', 'N', 'Times-Roman'
      ,  'H4sIAAAAAAAAC8WSKxLCQAyG+3Bopo4bVHbwHGCvUNNT9AB4JEwvgUBimUF3wCNR'
      || 'qAoGRZL9twlQikR8kzTvZBtF0SP6O7Ej1kTnSRfEhHw7+Jy3J4XGi8w05yeZh2sE'
      || '4j312ZDeEg1gvSJy6C36L9WX1urr4xrolfrSrYmrUCeDPGMu5+cQ3Ur3OXvQ+TYf'
      || '+2FGexOZvTM1L3S3o5fJjGQJX2n68U2ur3X5m3cTvfbxsk9pcsMee60rdTjnhNkc'
      || 'Zip9HOv9+7/tI3Oif3InOdV/oLdx3gq2HIRaB1Ob7XPk35QwwxDyxg3e09Dv6nSf'
      || 'rxQjvty8ywDce9CXvdF9R+4y4o+7J1P/I9sABAAA' );
--
    init_core_font( 6, 'times', 'I', 'Times-Italic'
      ,  'H4sIAAAAAAAAC8WSPQ6CQBCFF+i01NB5g63tPcBegYZTeAB6SxNLjLUH4BTEeAYr'
      || 'Kwpj5ezsW2YgoKXFl2Hnb9+wY4x5m7+TOOJMdIFsRywodkfMBX9aSz7bXGp+gj6+'
      || 'R4TvOtJ3CU5Eq85tgGsbxG3QN8iFZY1WzpxXwkckFTR7e1G6osZGWT1bDuBnTeP5'
      || 'KtW/E71c0yB2IFbBphuyBXIL9Y/9fPvhf8se6vsa8nmeQtU6NSf6ch9fc8P9DpqK'
      || 'cPa5/I7VxDwruTN9kV3LDvQ+h1m8z4I4x9LIbnn/Fv6nwOdyGq+d33jk7/cxztyq'
      || 'XRhTz/it7Mscg7fT5CO+9ahnYk20Hww5IrwABAAA' );
--
    init_core_font( 7, 'times', 'B', 'Times-Bold'
      , 'H4sIAAAAAAAAC8VSuw3CQAy9XBqUAVKxAZkgHQUNEiukySxpqOjTMQEDZIrUDICE'
      || 'RHUVVfy9c0IQJcWTfbafv+ece7u/Izs553cgAyN/APagl+wjgN3XKZ5kmTg/IXkw'
      || 'h4JqXUEfAb1I1VvwFYysk9iCffmN4+gtccSr5nlwDpuTepCZ/MH0FZibDUnO7MoR'
      || 'HXdDuvgjpzNxgevG+dF/hr3dWfoNyEZ8Taqn+7d7ozmqpGM8zdMYruFrXopVjvY2'
      || 'in9gXe+5vBf1KfX9E6TOVBsb8i5iqwQyv9+a3Gg/Cv+VoDtaQ7xdPwfNYRDji09g'
      || 'X/FvLNGmO62B9jSsoFwgfM+jf1z/SPwrkTMBOkCTBQAEAAA=' );
--
    init_core_font( 8, 'times', 'BI', 'Times-BoldItalic'
      ,  'H4sIAAAAAAAAC8WSuw2DMBCGHegYwEuECajIAGwQ0TBFBnCfPktkAKagzgCRIqWi'
      || 'oso9fr+Qo5RB+nT2ve+wMWYzf+fgjKmOJFelPhENnS0xANJXHfwHSBtjfoI8nMMj'
      || 'tXo63xKW/Cx9ONRn3US6C/wWvYeYNr+LH2IY6cHGPkJfvsc5kX7mFjF+Vqs9iT6d'
      || 'zwEL26y1Qz62nWlvD5VSf4R9zPuon/ne+C45+XxXf5lnTGLTOZCXPx8v9Qfdjdid'
      || '5vD/f/+/pE/Ur14kG+xjTHRc84pZWsC2Hjk2+Hgbx78j4Z8W4DlL+rBnEN5Bie6L'
      || 'fsL+1u/InuYCdsdaeAs+RxftKfGdfQDlDF/kAAQAAA==' );
--
    init_core_font( 9, 'courier', 'N', 'Courier', null );
    for i in 0 .. 255
    loop
      g_fonts( 9 ).char_width_tab( i ) := 600;
    end loop;
--
    init_core_font( 10, 'courier', 'I', 'Courier-Oblique', null );
    g_fonts( 10 ).char_width_tab := g_fonts( 9 ).char_width_tab;
--
    init_core_font( 11, 'courier', 'B', 'Courier-Bold', null );
    g_fonts( 11 ).char_width_tab := g_fonts( 9 ).char_width_tab;
--
    init_core_font( 12, 'courier', 'BI', 'Courier-BoldOblique', null );
    g_fonts( 12 ).char_width_tab := g_fonts( 9 ).char_width_tab;
--
    init_core_font( 13, 'symbol', 'N', 'Symbol'
      ,  'H4sIAAAAAAAAC82SIU8DQRCFZ28xIE+cqcbha4tENKk/gQCJJ6AweIK9H1CHqKnp'
      || 'D2gTFBaDIcFwCQkJSTG83fem7SU0qYNLvry5nZ25t7NnZkv7c8LQrFhAP6GHZvEY'
      || 'HOB9ylxGubTfNVRc34mKpFonzBQ/gUZ6Ds7AN6i5lv1dKv8Ab1eKQYSV4hUcgZFq'
      || 'J/Sec7fQHtdTn3iqfvdrb7m3e2pZW+xDG3oIJ/Li3gfMr949rlU74DyT1/AuTX1f'
      || 'YGhOzTP8B0/RggsEX/I03vgXPrrslZjfM8/pGu40t2ZjHgud97F7337mXP/GO4h9'
      || '3WmPPaOJ/jrOs9yC52MlrtUzfWupfTX51X/L+13Vl/J/s4W2S3pSfSh5DmeXerMf'
      || '+LXhWQAEAAA=' );
--
    init_core_font( 14, 'zapfdingbats', 'N', 'ZapfDingbats'
      ,  'H4sIAAAAAAAAC83ROy9EQRjG8TkzjdJl163SSHR0EpdsVkSi2UahFhUljUKUIgoq'
      || 'CrvJCtFQyG6EbSSERGxhC0ofQAQFxbIi8T/7PoUPIOEkvzxzzsycdy7O/fUTtToX'
      || 'bnCuvHPOV8gk4r423ovkGQ5od5OTWMeesmBz/RuZIWv4wCAY4z/xjipeqflC9qAD'
      || 'aRwxrxkJievSFzrRh36tZ1zttL6nkGX+A27xrLnttE/IBji9x7UvcIl9nPJ9AL36'
      || 'd1L9hyihoDW10L62cwhNyhntryZVExYl3kMj+zym+CrJv6M8VozPmfr5L8uwJORL'
      || 'tox7NFHG/Obj79FlwhqZ1X292xn6CbAXP/fjjv6rJYyBtUdl1vxEO6fcRB7bMmJ3'
      || 'GYZsTN0GdrDL/Ao5j1GZNr5kwqydX5z1syoiYEq5gCtlSrXi+mVbi3PfVAuhoQAE'
      || 'AAA=' );
--
  end;
  
  function add_object( p_txt varchar2 := null )
  return number
  is
    t_self number(10);
  begin
    p_newobj();
    t_self := n;
    --
    if p_txt is not null then
      txt2pdfdoc('<<' || p_txt || '>>' || c_nl || 'endobj' );
    end if;
--
    return t_self;
  end;
--
  procedure add_object( p_txt varchar2 := null )
  is
    t_dummy number(10) := add_object( p_txt );
  begin
    null;
  end;
--  
  function to_char_round
    ( p_value number
    , p_precision pls_integer := 2
    )
    return varchar2
  is
  begin
    return to_char( round( p_value, p_precision ), 'TM9', 'NLS_NUMERIC_CHARACTERS=.,' );
  end;  
  
    function flate_encode( p_val blob )
  return blob
  is
    t_blob blob;
  begin
    t_blob := hextoraw( '789C' );
    dbms_lob.copy( t_blob
                 , utl_compress.lz_compress( p_val )
                 , dbms_lob.lobmaxsize
                 , 3
                 , 11
                 );
    dbms_lob.trim( t_blob, dbms_lob.getlength( t_blob ) - 8 );
    dbms_lob.append( t_blob, hextoraw( adler32( p_val ) ) );
    return t_blob;
  end;
  
  procedure put_stream
    ( p_stream blob
    , p_compress boolean := true
    , p_extra varchar2 := ''
    , p_tag boolean := true
    )
  is
    t_blob blob;
    t_compress boolean := false;
  begin
    if p_compress and nvl( dbms_lob.getlength( p_stream ), 0 ) > 0
    then
      t_compress := true;
      t_blob := flate_encode( p_stream );
    else
      t_blob := p_stream;
    end if;
    txt2pdfdoc( case when p_tag then '<<' end
                || case when t_compress then '/Filter /FlateDecode ' end
                || '/Length ' || nvl( length( t_blob ), 0 )
                || p_extra
                || '>>' );
    txt2pdfdoc( 'stream' );
    raw2pdfdoc( t_blob );
    txt2pdfdoc( 'endstream' );
    if dbms_lob.istemporary( t_blob ) = 1
    then
      dbms_lob.freetemporary( t_blob );
    end if;
  end;
--
  function add_stream
    ( p_stream blob
    , p_extra varchar2 := ''
    , p_compress boolean := true
    )
  return number
  is
    t_self number(10);
  begin
    t_self := add_object;
    put_stream( p_stream
              , p_compress
              , p_extra
              );
    txt2pdfdoc( 'endobj' );
    return t_self;
  end;  
  
  function subset_font( p_index pls_integer )
  return blob
  is
    t_tmp blob;
    t_header blob;
    t_tables blob;
    t_len pls_integer;
    t_code pls_integer;
    t_glyph pls_integer;
    t_offset pls_integer;
    t_factor pls_integer;
    t_unicode pls_integer;
    t_used_glyphs tp_pls_tab;
    t_fmt varchar2(10);
    t_utf16_charset varchar2(1000);
    t_raw raw(32767);
    t_v varchar2(32767);
    t_table_records raw(32767);
  begin
    if g_fonts( p_index ).cid
    then
      t_used_glyphs := g_fonts( p_index ).used_chars;
      t_used_glyphs( 0 ) := 0;
    else
      t_utf16_charset := substr( g_fonts( p_index ).charset, 1, instr( g_fonts( p_index ).charset, '.' ) ) || 'AL16UTF16';
      t_used_glyphs( 0 ) := 0;
      t_code := g_fonts( p_index ).used_chars.first;
      while t_code is not null
      loop
        t_unicode := to_number( rawtohex( utl_raw.convert( hextoraw( to_char( t_code, 'fm0x' ) )
                                                                    , t_utf16_charset
                                                                    , g_fonts( p_index ).charset  -- ???? database characterset ?????
                                                                    )
                                        ), 'XXXXXXXX' );
        if g_fonts( p_index ).flags = 4 -- a symbolic font
        then
-- assume code 32, space maps to the first code from the font
          t_used_glyphs( g_fonts( p_index ).code2glyph( g_fonts( p_index ).code2glyph.first + t_unicode - 32 ) ) := 0;
        else
          t_used_glyphs( g_fonts( p_index ).code2glyph( t_unicode ) ) := 0;
        end if;
        t_code := g_fonts( p_index ).used_chars.next( t_code );
      end loop;
    end if;
--
    dbms_lob.createtemporary( t_tables, true );
    t_header := utl_raw.concat( hextoraw( '00010000' )
                              , dbms_lob.substr( g_fonts( p_index ).fontfile2, 8, g_fonts( p_index ).ttf_offset + 4 )
                              );
    t_offset := 12 + blob2num( g_fonts( p_index ).fontfile2, 2, g_fonts( p_index ).ttf_offset + 4 ) * 16;
    t_table_records := dbms_lob.substr( g_fonts( p_index ).fontfile2
                                      , blob2num( g_fonts( p_index ).fontfile2, 2, g_fonts( p_index ).ttf_offset + 4 ) * 16
                                      , g_fonts( p_index ).ttf_offset + 12
                                      );
    for i in 1 .. blob2num( g_fonts( p_index ).fontfile2, 2, g_fonts( p_index ).ttf_offset + 4 )
    loop
      case utl_raw.cast_to_varchar2( utl_raw.substr( t_table_records, i * 16 - 15, 4 ) )
        when 'post'
        then
          dbms_lob.append( t_header
                         , utl_raw.concat( utl_raw.substr( t_table_records, i * 16 - 15, 4 ) -- tag
                                         , hextoraw( '00000000' ) -- checksum
                                         , num2raw( t_offset + dbms_lob.getlength( t_tables ) ) -- offset
                                         , num2raw( 32 ) -- length
                                         )
                         );
          dbms_lob.append( t_tables
                         , utl_raw.concat( hextoraw( '00030000' )
                                         , dbms_lob.substr( g_fonts( p_index ).fontfile2
                                                          , 28
                                                          , raw2num( t_table_records, i * 16 - 7, 4 ) + 5
                                                          )
                                         )
                         );
        when 'loca'
        then
          if g_fonts( p_index ).indexToLocFormat = 0
          then
            t_fmt := 'fm0XXX';
          else
            t_fmt := 'fm0XXXXXXX';
          end if;
          t_raw := null;
          dbms_lob.createtemporary( t_tmp, true );
          t_len := 0;
          for g in 0 .. g_fonts( p_index ).numGlyphs - 1
          loop
            t_raw := utl_raw.concat( t_raw, hextoraw( to_char( t_len, t_fmt ) ) );
            if utl_raw.length( t_raw ) > 32770
            then
              dbms_lob.append( t_tmp, t_raw );
              t_raw := null;
            end if;
            if t_used_glyphs.exists( g )
            then
              t_len := t_len + g_fonts( p_index ).loca( g + 1 ) - g_fonts( p_index ).loca( g );
            end if;
          end loop;
          t_raw := utl_raw.concat( t_raw, hextoraw( to_char( t_len, t_fmt ) ) );
          dbms_lob.append( t_tmp, t_raw );
          dbms_lob.append( t_header
                         , utl_raw.concat( utl_raw.substr( t_table_records, i * 16 - 15, 4 ) -- tag
                                         , hextoraw( '00000000' ) -- checksum
                                         , num2raw( t_offset + dbms_lob.getlength( t_tables ) ) -- offset
                                         , num2raw( dbms_lob.getlength( t_tmp ) ) -- length
                                         )
                         );
          dbms_lob.append( t_tables, t_tmp );
          dbms_lob.freetemporary( t_tmp );
        when 'glyf'
        then
          if g_fonts( p_index ).indexToLocFormat = 0
          then
            t_factor := 2;
          else
            t_factor := 1;
          end if;
          t_raw := null;
          dbms_lob.createtemporary( t_tmp, true );
          for g in 0 .. g_fonts( p_index ).numGlyphs - 1
          loop
            if (   t_used_glyphs.exists( g )
               and g_fonts( p_index ).loca( g + 1 ) > g_fonts( p_index ).loca( g )
               )
            then
              t_raw := utl_raw.concat( t_raw
                                     , dbms_lob.substr( g_fonts( p_index ).fontfile2
                                                      , ( g_fonts( p_index ).loca( g + 1 ) - g_fonts( p_index ).loca( g ) ) * t_factor
                                                      , g_fonts( p_index ).loca( g ) * t_factor + raw2num( t_table_records, i * 16 - 7, 4 ) + 1
                                                      )
                                     );
              if utl_raw.length( t_raw ) > 32778
              then
                dbms_lob.append( t_tmp, t_raw );
                t_raw := null;
              end if;
            end if;
          end loop;
          if utl_raw.length( t_raw ) > 0
          then
            dbms_lob.append( t_tmp, t_raw );
          end if;
          dbms_lob.append( t_header
                         , utl_raw.concat( utl_raw.substr( t_table_records, i * 16 - 15, 4 ) -- tag
                                         , hextoraw( '00000000' ) -- checksum
                                         , num2raw( t_offset + dbms_lob.getlength( t_tables ) ) -- offset
                                         , num2raw( dbms_lob.getlength( t_tmp ) ) -- length
                                         )
                         );
          dbms_lob.append( t_tables, t_tmp );
          dbms_lob.freetemporary( t_tmp );
        else
          dbms_lob.append( t_header
                         , utl_raw.concat( utl_raw.substr( t_table_records, i * 16 - 15, 4 )    -- tag
                                         , utl_raw.substr( t_table_records, i * 16 - 11, 4 )    -- checksum
                                         , num2raw( t_offset + dbms_lob.getlength( t_tables ) ) -- offset
                                         , utl_raw.substr( t_table_records, i * 16 - 3, 4 )     -- length
                                         )
                         );
          dbms_lob.copy( t_tables
                       , g_fonts( p_index ).fontfile2
                       , raw2num( t_table_records, i * 16 - 3, 4 )
                       , dbms_lob.getlength( t_tables ) + 1
                       , raw2num( t_table_records, i * 16 - 7, 4 ) + 1
                       );
      end case;
    end loop;
    dbms_lob.append( t_header, t_tables );
    dbms_lob.freetemporary( t_tables );
    return t_header;
  end;  
  
  function add_font( p_index pls_integer )
  return number
  is
    t_self number(10);
    t_fontfile number(10);
    t_font_subset blob;
    t_used pls_integer;
    t_used_glyphs tp_pls_tab;
    t_w varchar2(32767);
    t_unicode pls_integer;
    t_utf16_charset varchar2(1000);
    t_width number;
  begin
    if g_fonts( p_index ).standard
    then
      return add_object( '/Type/Font'
                       || '/Subtype/Type1'
                       || '/BaseFont/' || g_fonts( p_index ).name
                       || '/Encoding/WinAnsiEncoding' -- code page 1252
                       );
    end if;
--
    if g_fonts( p_index ).cid
    then
      t_self := add_object;
      txt2pdfdoc( '<</Type/Font/Subtype/Type0/Encoding/Identity-H'
                || '/BaseFont/' || g_fonts( p_index ).name
                || '/DescendantFonts ' || to_char( t_self + 1 ) || ' 0 R'
                || '/ToUnicode ' || to_char( t_self + 8 ) || ' 0 R'
                || '>>' );
      txt2pdfdoc( 'endobj' );
      add_object;
      txt2pdfdoc( '[' || to_char( t_self + 2 ) || ' 0 R]' );
      txt2pdfdoc( 'endobj' );
      add_object( '/Type/Font/Subtype/CIDFontType2/CIDToGIDMap/Identity/DW 1000'
                || '/BaseFont/' || g_fonts( p_index ).name
                || '/CIDSystemInfo ' || to_char( t_self + 3 ) || ' 0 R'
                || '/W ' || to_char( t_self + 4 ) || ' 0 R'
                || '/FontDescriptor ' || to_char( t_self + 5 ) || ' 0 R' );
      add_object( '/Ordering(Identity) /Registry(Adobe) /Supplement 0' );
--
      t_utf16_charset := substr( g_fonts( p_index ).charset, 1, instr( g_fonts( p_index ).charset, '.' ) ) || 'AL16UTF16';
      t_used_glyphs := g_fonts( p_index ).used_chars;
      t_used_glyphs( 0 ) := 0;
      t_used := t_used_glyphs.first();
      while t_used is not null
      loop
        if g_fonts( p_index ).hmetrics.exists( t_used )
        then
          t_width := g_fonts( p_index ).hmetrics( t_used );
        else
          t_width := g_fonts( p_index ).hmetrics( g_fonts( p_index ).hmetrics.last() );
        end if;
        t_width := trunc( t_width * g_fonts( p_index ).unit_norm );
        if t_used_glyphs.prior( t_used ) = t_used - 1
        then
          t_w := t_w || ' ' || t_width;
        else
          t_w := t_w || '] ' || t_used || ' [' || t_width;
        end if;
        t_used := t_used_glyphs.next( t_used );
      end loop;
      t_w := '[' || ltrim( t_w, '] ' ) || ']]';
      add_object;
      txt2pdfdoc( t_w );
      txt2pdfdoc( 'endobj' );
      add_object
        (    '/Type/FontDescriptor'
          || '/FontName/' || g_fonts( p_index ).name
          || '/Flags ' || g_fonts( p_index ).flags
          || '/FontBBox [' || g_fonts( p_index ).bb_xmin
          || ' ' || g_fonts( p_index ).bb_ymin
          || ' ' || g_fonts( p_index ).bb_xmax
          || ' ' || g_fonts( p_index ).bb_ymax
          || ']'
          || '/ItalicAngle ' || to_char_round( g_fonts( p_index ).italic_angle )
          || '/Ascent ' || g_fonts( p_index ).ascent
          || '/Descent ' || g_fonts( p_index ).descent
          || '/CapHeight ' || g_fonts( p_index ).capheight
          || '/StemV ' || g_fonts( p_index ).stemv
          || '/FontFile2 ' || to_char( t_self + 6 ) || ' 0 R' );
      t_fontfile := add_stream( g_fonts( p_index ).fontfile2
                              , '/Length1 ' || dbms_lob.getlength( g_fonts( p_index ).fontfile2 )
                              , g_fonts( p_index ).compress_font
                              );
      t_font_subset := subset_font( p_index );
      t_fontfile := add_stream( t_font_subset
                              , '/Length1 ' || dbms_lob.getlength( t_font_subset )
                              , g_fonts( p_index ).compress_font
                              );
      declare
        t_g2c tp_pls_tab;
        t_code     pls_integer;
        t_c_start  pls_integer;
        t_map varchar2(32767);
        t_cmap varchar2(32767);
        t_cor pls_integer;
        t_cnt pls_integer;
      begin
        t_code := g_fonts( p_index ).code2glyph.first;
        if g_fonts( p_index ).flags = 4 -- a symbolic font
        then
-- assume code 32, space maps to the first code from the font
          t_cor := t_code - 32;
        else
          t_cor := 0;
        end if;
        while t_code is not null
        loop
          t_g2c( g_fonts( p_index ).code2glyph( t_code ) ) := t_code - t_cor;
          t_code := g_fonts( p_index ).code2glyph.next( t_code );
        end loop;
        t_cnt := 0;
        t_used_glyphs := g_fonts( p_index ).used_chars;
        t_used := t_used_glyphs.first();
        while t_used is not null
        loop
          t_map := t_map || '<' || to_char( t_used, 'FM0XXX' )
                 || '> <' || to_char( t_g2c( t_used ), 'FM0XXX' )
                 || '>' || chr( 10 );
          if t_cnt = 99
          then
            t_cnt := 0;
            t_cmap := t_cmap || chr( 10 ) || '100 beginbfchar' || chr( 10 ) || t_map || 'endbfchar';
            t_map := '';
          else
            t_cnt := t_cnt + 1;
          end if;
          t_used := t_used_glyphs.next( t_used );
        end loop;
        if t_cnt > 0
        then
          t_cmap := t_cnt || ' beginbfchar' || chr( 10 ) || t_map || 'endbfchar';
        end if;
        t_fontfile := add_stream( utl_raw.cast_to_raw(
'/CIDInit /ProcSet findresource begin 12 dict begin
begincmap
/CIDSystemInfo
<< /Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def
/CMapName /Adobe-Identity-UCS def /CMapType 2 def
1 begincodespacerange
<0000> <FFFF>
endcodespacerange
' || t_cmap || '
endcmap
CMapName currentdict /CMap defineresource pop
end
end' ) );
      end;
      return t_self;
    end if;
--
    g_fonts( p_index ).first_char := g_fonts( p_index ).used_chars.first();
    g_fonts( p_index ).last_char := g_fonts( p_index ).used_chars.last();
    t_self := add_object;
    txt2pdfdoc( '<</Type /Font '
              || '/Subtype /' || g_fonts( p_index ).subtype
              || ' /BaseFont /' || g_fonts( p_index ).name
              || ' /FirstChar ' || g_fonts( p_index ).first_char
              || ' /LastChar ' || g_fonts( p_index ).last_char
              || ' /Widths ' || to_char( t_self + 1 ) || ' 0 R'
              || ' /FontDescriptor ' || to_char( t_self + 2 ) || ' 0 R'
              || ' /Encoding ' || to_char( t_self + 3 ) || ' 0 R'
              || ' >>' );
    txt2pdfdoc( 'endobj' );
    add_object;
    txt2pdfdoc( '[' );
      begin
        for i in g_fonts( p_index ).first_char .. g_fonts( p_index ).last_char
        loop
          txt2pdfdoc( g_fonts( p_index ).char_width_tab( i ) );
        end loop;
      exception
        when others
        then
          dbms_output.put_line( '**** ' || g_fonts( p_index ).name );
      end;
      txt2pdfdoc( ']' );
      txt2pdfdoc( 'endobj' );
      add_object
        (    '/Type /FontDescriptor'
          || ' /FontName /' || g_fonts( p_index ).name
          || ' /Flags ' || g_fonts( p_index ).flags
          || ' /FontBBox [' || g_fonts( p_index ).bb_xmin
          || ' ' || g_fonts( p_index ).bb_ymin
          || ' ' || g_fonts( p_index ).bb_xmax
          || ' ' || g_fonts( p_index ).bb_ymax
          || ']'
          || ' /ItalicAngle ' || to_char_round( g_fonts( p_index ).italic_angle )
          || ' /Ascent ' || g_fonts( p_index ).ascent
          || ' /Descent ' || g_fonts( p_index ).descent
          || ' /CapHeight ' || g_fonts( p_index ).capheight
          || ' /StemV ' || g_fonts( p_index ).stemv
          || case
               when g_fonts( p_index ).fontfile2 is not null
                 then ' /FontFile2 ' || to_char( t_self + 4 ) || ' 0 R'
             end );
      add_object(    '/Type /Encoding /BaseEncoding /WinAnsiEncoding '
                         || g_fonts( p_index ).diff
                         || ' ' );
      if g_fonts( p_index ).fontfile2 is not null
      then
        t_font_subset := subset_font( p_index );
        t_fontfile :=
          add_stream( t_font_subset
                    , '/Length1 ' || dbms_lob.getlength( t_font_subset )
                    , g_fonts( p_index ).compress_font
                    );
    end if;
    return t_self;
  end;    
 
  function load_ttf_font
    ( p_font blob
    , p_encoding varchar2 := 'WINDOWS-1252'
    , p_embed boolean := false
    , p_compress boolean := true
    , p_offset number := 1
    )
  return pls_integer
  is
    this_font tp_font;
    type tp_font_table is record
      ( offset pls_integer
      , length pls_integer
      );
    type tp_tables is table of tp_font_table index by varchar2(4);
    t_tables tp_tables;
    t_tag varchar2(4);
    t_blob blob;
    t_offset pls_integer;
    nr_hmetrics pls_integer;
    subtype tp_glyphname is varchar2(250);
    type tp_glyphnames is table of tp_glyphname index by pls_integer;
    t_glyphnames tp_glyphnames;
    t_glyph2name tp_pls_tab;
    t_font_ind pls_integer;
  begin
  dbms_output.put_line( 'Starting loading....');
    if dbms_lob.substr( p_font, 4, p_offset ) != hextoraw( '00010000' ) --  OpenType Font
    then
      return null;
    end if;
    for i in 1 .. blob2num( p_font, 2, p_offset + 4 )
    loop
      t_tag :=
        utl_raw.cast_to_varchar2( dbms_lob.substr( p_font, 4, p_offset - 4 + i * 16 ) );
      t_tables( t_tag ).offset := blob2num( p_font, 4, p_offset + 4 + i * 16 ) + 1;
      t_tables( t_tag ).length := blob2num( p_font, 4, p_offset + 8 + i * 16 );
    end loop;
--
    if (  not t_tables.exists( 'cmap' )
       or not t_tables.exists( 'glyf' )
       or not t_tables.exists( 'head' )
       or not t_tables.exists( 'hhea' )
       or not t_tables.exists( 'hmtx' )
       or not t_tables.exists( 'loca' )
       or not t_tables.exists( 'maxp' )
       or not t_tables.exists( 'name' )
       or not t_tables.exists( 'post' )
       )
    then
      return null;
    end if;
--
    dbms_lob.createtemporary( t_blob, true );
    dbms_lob.copy( t_blob, p_font, t_tables( 'maxp' ).length, 1, t_tables( 'maxp' ).offset );
    this_font.numGlyphs := blob2num( t_blob, 2, 5 );
--
    dbms_lob.copy( t_blob, p_font, t_tables( 'cmap' ).length, 1, t_tables( 'cmap' ).offset );
    for i in 0 .. blob2num( t_blob, 2, 3 ) - 1
    loop
      if (   dbms_lob.substr( t_blob, 2, 5 + i * 8 ) = hextoraw( '0003' ) -- Windows
         and dbms_lob.substr( t_blob, 2, 5 + i * 8 + 2 )
               in ( hextoraw( '0000' ) -- Symbol
                  , hextoraw( '0001' ) -- Unicode BMP (UCS-2)
                  )
         )
      then
        if dbms_lob.substr( t_blob, 2, 5 + i * 8 + 2 ) = hextoraw( '0000' ) -- Symbol
        then
          this_font.flags := 4; -- symbolic
        else
          this_font.flags := 32; -- non-symbolic
        end if;
        t_offset := blob2num( t_blob, 4, 5 + i * 8 + 4 ) + 1;
        if dbms_lob.substr( t_blob, 2, t_offset ) != hextoraw( '0004' )
        then
          return null;
        end if;
        declare
          t_seg_cnt pls_integer;
          t_end_offs pls_integer;
          t_start_offs pls_integer;
          t_idDelta_offs pls_integer;
          t_idRangeOffset_offs pls_integer;
          t_tmp pls_integer;
          t_start pls_integer;
        begin
          t_seg_cnt := blob2num( t_blob, 2, t_offset + 6 ) / 2;
          t_end_offs := t_offset + 14;
          t_start_offs := t_end_offs + t_seg_cnt * 2 + 2;
          t_idDelta_offs := t_start_offs + t_seg_cnt * 2;
          t_idRangeOffset_offs := t_idDelta_offs + t_seg_cnt * 2;
          for seg in 0 .. t_seg_cnt - 1
          loop
            t_tmp := blob2num( t_blob, 2, t_idRangeOffset_offs + seg * 2 );
            if t_tmp = 0
            then
              t_tmp := blob2num( t_blob, 2, t_idDelta_offs + seg * 2 );
              for c in blob2num( t_blob, 2, t_start_offs + seg * 2 )
                    .. blob2num( t_blob, 2, t_end_offs + seg * 2 )
              loop
                this_font.code2glyph( c ) := mod( c + t_tmp, 65536 );
              end loop;
            else
              t_start := blob2num( t_blob, 2, t_start_offs + seg * 2 );
              for c in t_start .. blob2num( t_blob, 2, t_end_offs + seg * 2 )
              loop
                this_font.code2glyph( c ) := blob2num( t_blob, 2, t_idRangeOffset_offs + t_tmp + ( seg + c - t_start ) * 2 );
              end loop;
            end if;
          end loop;
        end;
        exit;
      end if;
    end loop;
--
    t_glyphnames( 0 ) := '.notdef';
    t_glyphnames( 1 ) := '.null';
    t_glyphnames( 2 ) := 'nonmarkingreturn';
    t_glyphnames( 3 ) := 'space';
    t_glyphnames( 4 ) := 'exclam';
    t_glyphnames( 5 ) := 'quotedbl';
    t_glyphnames( 6 ) := 'numbersign';
    t_glyphnames( 7 ) := 'dollar';
    t_glyphnames( 8 ) := 'percent';
    t_glyphnames( 9 ) := 'ampersand';
    t_glyphnames( 10 ) := 'quotesingle';
    t_glyphnames( 11 ) := 'parenleft';
    t_glyphnames( 12 ) := 'parenright';
    t_glyphnames( 13 ) := 'asterisk';
    t_glyphnames( 14 ) := 'plus';
    t_glyphnames( 15 ) := 'comma';
    t_glyphnames( 16 ) := 'hyphen';
    t_glyphnames( 17 ) := 'period';
    t_glyphnames( 18 ) := 'slash';
    t_glyphnames( 19 ) := 'zero';
    t_glyphnames( 20 ) := 'one';
    t_glyphnames( 21 ) := 'two';
    t_glyphnames( 22 ) := 'three';
    t_glyphnames( 23 ) := 'four';
    t_glyphnames( 24 ) := 'five';
    t_glyphnames( 25 ) := 'six';
    t_glyphnames( 26 ) := 'seven';
    t_glyphnames( 27 ) := 'eight';
    t_glyphnames( 28 ) := 'nine';
    t_glyphnames( 29 ) := 'colon';
    t_glyphnames( 30 ) := 'semicolon';
    t_glyphnames( 31 ) := 'less';
    t_glyphnames( 32 ) := 'equal';
    t_glyphnames( 33 ) := 'greater';
    t_glyphnames( 34 ) := 'question';
    t_glyphnames( 35 ) := 'at';
    t_glyphnames( 36 ) := 'A';
    t_glyphnames( 37 ) := 'B';
    t_glyphnames( 38 ) := 'C';
    t_glyphnames( 39 ) := 'D';
    t_glyphnames( 40 ) := 'E';
    t_glyphnames( 41 ) := 'F';
    t_glyphnames( 42 ) := 'G';
    t_glyphnames( 43 ) := 'H';
    t_glyphnames( 44 ) := 'I';
    t_glyphnames( 45 ) := 'J';
    t_glyphnames( 46 ) := 'K';
    t_glyphnames( 47 ) := 'L';
    t_glyphnames( 48 ) := 'M';
    t_glyphnames( 49 ) := 'N';
    t_glyphnames( 50 ) := 'O';
    t_glyphnames( 51 ) := 'P';
    t_glyphnames( 52 ) := 'Q';
    t_glyphnames( 53 ) := 'R';
    t_glyphnames( 54 ) := 'S';
    t_glyphnames( 55 ) := 'T';
    t_glyphnames( 56 ) := 'U';
    t_glyphnames( 57 ) := 'V';
    t_glyphnames( 58 ) := 'W';
    t_glyphnames( 59 ) := 'X';
    t_glyphnames( 60 ) := 'Y';
    t_glyphnames( 61 ) := 'Z';
    t_glyphnames( 62 ) := 'bracketleft';
    t_glyphnames( 63 ) := 'backslash';
    t_glyphnames( 64 ) := 'bracketright';
    t_glyphnames( 65 ) := 'asciicircum';
    t_glyphnames( 66 ) := 'underscore';
    t_glyphnames( 67 ) := 'grave';
    t_glyphnames( 68 ) := 'a';
    t_glyphnames( 69 ) := 'b';
    t_glyphnames( 70 ) := 'c';
    t_glyphnames( 71 ) := 'd';
    t_glyphnames( 72 ) := 'e';
    t_glyphnames( 73 ) := 'f';
    t_glyphnames( 74 ) := 'g';
    t_glyphnames( 75 ) := 'h';
    t_glyphnames( 76 ) := 'i';
    t_glyphnames( 77 ) := 'j';
    t_glyphnames( 78 ) := 'k';
    t_glyphnames( 79 ) := 'l';
    t_glyphnames( 80 ) := 'm';
    t_glyphnames( 81 ) := 'n';
    t_glyphnames( 82 ) := 'o';
    t_glyphnames( 83 ) := 'p';
    t_glyphnames( 84 ) := 'q';
    t_glyphnames( 85 ) := 'r';
    t_glyphnames( 86 ) := 's';
    t_glyphnames( 87 ) := 't';
    t_glyphnames( 88 ) := 'u';
    t_glyphnames( 89 ) := 'v';
    t_glyphnames( 90 ) := 'w';
    t_glyphnames( 91 ) := 'x';
    t_glyphnames( 92 ) := 'y';
    t_glyphnames( 93 ) := 'z';
    t_glyphnames( 94 ) := 'braceleft';
    t_glyphnames( 95 ) := 'bar';
    t_glyphnames( 96 ) := 'braceright';
    t_glyphnames( 97 ) := 'asciitilde';
    t_glyphnames( 98 ) := 'Adieresis';
    t_glyphnames( 99 ) := 'Aring';
    t_glyphnames( 100 ) := 'Ccedilla';
    t_glyphnames( 101 ) := 'Eacute';
    t_glyphnames( 102 ) := 'Ntilde';
    t_glyphnames( 103 ) := 'Odieresis';
    t_glyphnames( 104 ) := 'Udieresis';
    t_glyphnames( 105 ) := 'aacute';
    t_glyphnames( 106 ) := 'agrave';
    t_glyphnames( 107 ) := 'acircumflex';
    t_glyphnames( 108 ) := 'adieresis';
    t_glyphnames( 109 ) := 'atilde';
    t_glyphnames( 110 ) := 'aring';
    t_glyphnames( 111 ) := 'ccedilla';
    t_glyphnames( 112 ) := 'eacute';
    t_glyphnames( 113 ) := 'egrave';
    t_glyphnames( 114 ) := 'ecircumflex';
    t_glyphnames( 115 ) := 'edieresis';
    t_glyphnames( 116 ) := 'iacute';
    t_glyphnames( 117 ) := 'igrave';
    t_glyphnames( 118 ) := 'icircumflex';
    t_glyphnames( 119 ) := 'idieresis';
    t_glyphnames( 120 ) := 'ntilde';
    t_glyphnames( 121 ) := 'oacute';
    t_glyphnames( 122 ) := 'ograve';
    t_glyphnames( 123 ) := 'ocircumflex';
    t_glyphnames( 124 ) := 'odieresis';
    t_glyphnames( 125 ) := 'otilde';
    t_glyphnames( 126 ) := 'uacute';
    t_glyphnames( 127 ) := 'ugrave';
    t_glyphnames( 128 ) := 'ucircumflex';
    t_glyphnames( 129 ) := 'udieresis';
    t_glyphnames( 130 ) := 'dagger';
    t_glyphnames( 131 ) := 'degree';
    t_glyphnames( 132 ) := 'cent';
    t_glyphnames( 133 ) := 'sterling';
    t_glyphnames( 134 ) := 'section';
    t_glyphnames( 135 ) := 'bullet';
    t_glyphnames( 136 ) := 'paragraph';
    t_glyphnames( 137 ) := 'germandbls';
    t_glyphnames( 138 ) := 'registered';
    t_glyphnames( 139 ) := 'copyright';
    t_glyphnames( 140 ) := 'trademark';
    t_glyphnames( 141 ) := 'acute';
    t_glyphnames( 142 ) := 'dieresis';
    t_glyphnames( 143 ) := 'notequal';
    t_glyphnames( 144 ) := 'AE';
    t_glyphnames( 145 ) := 'Oslash';
    t_glyphnames( 146 ) := 'infinity';
    t_glyphnames( 147 ) := 'plusminus';
    t_glyphnames( 148 ) := 'lessequal';
    t_glyphnames( 149 ) := 'greaterequal';
    t_glyphnames( 150 ) := 'yen';
    t_glyphnames( 151 ) := 'mu';
    t_glyphnames( 152 ) := 'partialdiff';
    t_glyphnames( 153 ) := 'summation';
    t_glyphnames( 154 ) := 'product';
    t_glyphnames( 155 ) := 'pi';
    t_glyphnames( 156 ) := 'integral';
    t_glyphnames( 157 ) := 'ordfeminine';
    t_glyphnames( 158 ) := 'ordmasculine';
    t_glyphnames( 159 ) := 'Omega';
    t_glyphnames( 160 ) := 'ae';
    t_glyphnames( 161 ) := 'oslash';
    t_glyphnames( 162 ) := 'questiondown';
    t_glyphnames( 163 ) := 'exclamdown';
    t_glyphnames( 164 ) := 'logicalnot';
    t_glyphnames( 165 ) := 'radical';
    t_glyphnames( 166 ) := 'florin';
    t_glyphnames( 167 ) := 'approxequal';
    t_glyphnames( 168 ) := 'Delta';
    t_glyphnames( 169 ) := 'guillemotleft';
    t_glyphnames( 170 ) := 'guillemotright';
    t_glyphnames( 171 ) := 'ellipsis';
    t_glyphnames( 172 ) := 'nonbreakingspace';
    t_glyphnames( 173 ) := 'Agrave';
    t_glyphnames( 174 ) := 'Atilde';
    t_glyphnames( 175 ) := 'Otilde';
    t_glyphnames( 176 ) := 'OE';
    t_glyphnames( 177 ) := 'oe';
    t_glyphnames( 178 ) := 'endash';
    t_glyphnames( 179 ) := 'emdash';
    t_glyphnames( 180 ) := 'quotedblleft';
    t_glyphnames( 181 ) := 'quotedblright';
    t_glyphnames( 182 ) := 'quoteleft';
    t_glyphnames( 183 ) := 'quoteright';
    t_glyphnames( 184 ) := 'divide';
    t_glyphnames( 185 ) := 'lozenge';
    t_glyphnames( 186 ) := 'ydieresis';
    t_glyphnames( 187 ) := 'Ydieresis';
    t_glyphnames( 188 ) := 'fraction';
    t_glyphnames( 189 ) := 'currency';
    t_glyphnames( 190 ) := 'guilsinglleft';
    t_glyphnames( 191 ) := 'guilsinglright';
    t_glyphnames( 192 ) := 'fi';
    t_glyphnames( 193 ) := 'fl';
    t_glyphnames( 194 ) := 'daggerdbl';
    t_glyphnames( 195 ) := 'periodcentered';
    t_glyphnames( 196 ) := 'quotesinglbase';
    t_glyphnames( 197 ) := 'quotedblbase';
    t_glyphnames( 198 ) := 'perthousand';
    t_glyphnames( 199 ) := 'Acircumflex';
    t_glyphnames( 200 ) := 'Ecircumflex';
    t_glyphnames( 201 ) := 'Aacute';
    t_glyphnames( 202 ) := 'Edieresis';
    t_glyphnames( 203 ) := 'Egrave';
    t_glyphnames( 204 ) := 'Iacute';
    t_glyphnames( 205 ) := 'Icircumflex';
    t_glyphnames( 206 ) := 'Idieresis';
    t_glyphnames( 207 ) := 'Igrave';
    t_glyphnames( 208 ) := 'Oacute';
    t_glyphnames( 209 ) := 'Ocircumflex';
    t_glyphnames( 210 ) := 'apple';
    t_glyphnames( 211 ) := 'Ograve';
    t_glyphnames( 212 ) := 'Uacute';
    t_glyphnames( 213 ) := 'Ucircumflex';
    t_glyphnames( 214 ) := 'Ugrave';
    t_glyphnames( 215 ) := 'dotlessi';
    t_glyphnames( 216 ) := 'circumflex';
    t_glyphnames( 217 ) := 'tilde';
    t_glyphnames( 218 ) := 'macron';
    t_glyphnames( 219 ) := 'breve';
    t_glyphnames( 220 ) := 'dotaccent';
    t_glyphnames( 221 ) := 'ring';
    t_glyphnames( 222 ) := 'cedilla';
    t_glyphnames( 223 ) := 'hungarumlaut';
    t_glyphnames( 224 ) := 'ogonek';
    t_glyphnames( 225 ) := 'caron';
    t_glyphnames( 226 ) := 'Lslash';
    t_glyphnames( 227 ) := 'lslash';
    t_glyphnames( 228 ) := 'Scaron';
    t_glyphnames( 229 ) := 'scaron';
    t_glyphnames( 230 ) := 'Zcaron';
    t_glyphnames( 231 ) := 'zcaron';
    t_glyphnames( 232 ) := 'brokenbar';
    t_glyphnames( 233 ) := 'Eth';
    t_glyphnames( 234 ) := 'eth';
    t_glyphnames( 235 ) := 'Yacute';
    t_glyphnames( 236 ) := 'yacute';
    t_glyphnames( 237 ) := 'Thorn';
    t_glyphnames( 238 ) := 'thorn';
    t_glyphnames( 239 ) := 'minus';
    t_glyphnames( 240 ) := 'multiply';
    t_glyphnames( 241 ) := 'onesuperior';
    t_glyphnames( 242 ) := 'twosuperior';
    t_glyphnames( 243 ) := 'threesuperior';
    t_glyphnames( 244 ) := 'onehalf';
    t_glyphnames( 245 ) := 'onequarter';
    t_glyphnames( 246 ) := 'threequarters';
    t_glyphnames( 247 ) := 'franc';
    t_glyphnames( 248 ) := 'Gbreve';
    t_glyphnames( 249 ) := 'gbreve';
    t_glyphnames( 250 ) := 'Idotaccent';
    t_glyphnames( 251 ) := 'Scedilla';
    t_glyphnames( 252 ) := 'scedilla';
    t_glyphnames( 253 ) := 'Cacute';
    t_glyphnames( 254 ) := 'cacute';
    t_glyphnames( 255 ) := 'Ccaron';
    t_glyphnames( 256 ) := 'ccaron';
    t_glyphnames( 257 ) := 'dcroat';
--
    dbms_lob.copy( t_blob, p_font, t_tables( 'post' ).length, 1, t_tables( 'post' ).offset );
    this_font.italic_angle := to_short( dbms_lob.substr( t_blob, 2, 5 ) )
                            + to_short( dbms_lob.substr( t_blob, 2, 7 ) ) / 65536;
    case rawtohex( dbms_lob.substr( t_blob, 4, 1 ) )
      when '00010000'
      then
        for g in 0 .. 257
        loop
          t_glyph2name( g ) := g;
        end loop;
      when '00020000'
      then
        t_offset := blob2num( t_blob, 2, 33 ) * 2 + 35;
        while nvl( blob2num( t_blob, 1, t_offset ), 0 ) > 0
        loop
          t_glyphnames( t_glyphnames.count ) := utl_raw.cast_to_varchar2( dbms_lob.substr( t_blob, blob2num( t_blob, 1, t_offset ), t_offset + 1 ) );
          t_offset := t_offset + blob2num( t_blob, 1, t_offset ) + 1;
        end loop;
        for g in 0 .. blob2num( t_blob, 2, 33 ) - 1
        loop
          t_glyph2name( g ) := blob2num( t_blob, 2, 35 + 2 * g );
        end loop;
      when '00025000'
      then
        for g in 0 .. blob2num( t_blob, 2, 33 ) - 1
        loop
          t_offset := blob2num( t_blob, 1, 35 + g );
          if t_offset > 127
          then
            t_glyph2name( g ) := g - t_offset;
          else
            t_glyph2name( g ) := g + t_offset;
          end if;
        end loop;
      when '00030000'
      then
        t_glyphnames.delete;
      else
dbms_output.put_line( 'no post ' || dbms_lob.substr( t_blob, 4, 1 ) );
    end case;
--
    dbms_lob.copy( t_blob, p_font, t_tables( 'head' ).length, 1, t_tables( 'head' ).offset );
    if dbms_lob.substr( t_blob, 4, 13 ) = hextoraw( '5F0F3CF5' )  -- magic
    then
      declare
        t_tmp pls_integer := blob2num( t_blob, 2, 45 );
      begin
        if bitand( t_tmp, 1 ) = 1
        then
          this_font.style := 'B';
        end if;
        if bitand( t_tmp, 2 ) = 2
        then
          this_font.style := this_font.style || 'I';
          this_font.flags := this_font.flags + 64;
        end if;
        this_font.style := nvl( this_font.style, 'N' );
        this_font.unit_norm := 1000 / blob2num( t_blob, 2, 19 );
        this_font.bb_xmin := to_short( dbms_lob.substr( t_blob, 2, 37 ), this_font.unit_norm );
        this_font.bb_ymin := to_short( dbms_lob.substr( t_blob, 2, 39 ), this_font.unit_norm );
        this_font.bb_xmax := to_short( dbms_lob.substr( t_blob, 2, 41 ), this_font.unit_norm );
        this_font.bb_ymax := to_short( dbms_lob.substr( t_blob, 2, 43 ), this_font.unit_norm );
        this_font.indexToLocFormat := blob2num( t_blob, 2, 51 ); -- 0 for short offsets, 1 for long
      end;
    end if;
--
    dbms_lob.copy( t_blob, p_font, t_tables( 'hhea' ).length, 1, t_tables( 'hhea' ).offset );
    if dbms_lob.substr( t_blob, 4, 1 ) = hextoraw( '00010000' ) -- version 1.0
    then
      this_font.ascent := to_short( dbms_lob.substr( t_blob, 2, 5 ), this_font.unit_norm );
      this_font.descent := to_short( dbms_lob.substr( t_blob, 2, 7 ), this_font.unit_norm );
      this_font.capheight := this_font.ascent;
      nr_hmetrics := blob2num( t_blob, 2, 35 );
    end if;
--
    dbms_lob.copy( t_blob, p_font, t_tables( 'hmtx' ).length, 1, t_tables( 'hmtx' ).offset );
    for j in 0 .. nr_hmetrics - 1
    loop
      this_font.hmetrics( j ) := blob2num( t_blob, 2, 1 + 4 * j );
    end loop;
--
    dbms_lob.copy( t_blob, p_font, t_tables( 'name' ).length, 1, t_tables( 'name' ).offset );
    if dbms_lob.substr( t_blob, 2, 1 ) = hextoraw( '0000' ) -- format 0
    then
      t_offset := blob2num( t_blob, 2, 5 ) + 1;
      for j in 0 .. blob2num( t_blob, 2, 3 ) - 1
      loop
        if (   dbms_lob.substr( t_blob, 2, 7  + j * 12 ) = hextoraw( '0003' ) -- Windows
           and dbms_lob.substr( t_blob, 2, 11 + j * 12 ) = hextoraw( '0409' ) -- English United States
           )
        then
          case rawtohex( dbms_lob.substr( t_blob, 2, 13 + j * 12 ) )
            when '0001'
            then
              this_font.family := utl_i18n.raw_to_char( dbms_lob.substr( t_blob, blob2num( t_blob, 2, 15 + j * 12 ), t_offset + blob2num( t_blob, 2, 17 + j * 12 ) ), 'AL16UTF16' );
            when '0006'
            then
              this_font.name := utl_i18n.raw_to_char( dbms_lob.substr( t_blob, blob2num( t_blob, 2, 15 + j * 12 ), t_offset + blob2num( t_blob, 2, 17 + j * 12 ) ), 'AL16UTF16' );
            else
              null;
          end case;
        end if;
      end loop;
    end if;
--
    if this_font.italic_angle != 0
    then
      this_font.flags := this_font.flags + 64;
    end if;
    this_font.subtype := 'TrueType';
    this_font.stemv := 50;
    this_font.family := lower( this_font.family );
    this_font.encoding := utl_i18n.map_charset( p_encoding
                                              , utl_i18n.generic_context
                                              , utl_i18n.iana_to_oracle
                                              );
    this_font.encoding := nvl( this_font.encoding, upper( p_encoding ) );
    this_font.charset := sys_context( 'userenv', 'LANGUAGE' );
    this_font.charset := substr( this_font.charset
                               , 1
                               , instr( this_font.charset, '.' )
                               ) || this_font.encoding;
    this_font.cid := upper( p_encoding ) in ( 'CID', 'AL16UTF16', 'UTF', 'UNICODE' );
    this_font.fontname := this_font.name;
    this_font.compress_font := p_compress;
--
    if ( p_embed or this_font.cid ) and t_tables.exists( 'OS/2' )
    then
      dbms_lob.copy( t_blob, p_font, t_tables( 'OS/2' ).length, 1, t_tables( 'OS/2' ).offset );
      if blob2num( t_blob, 2, 9 ) != 2
      then
        this_font.fontfile2 := p_font;
        this_font.ttf_offset := p_offset;
        this_font.name := dbms_random.string( 'u', 6 ) || '+' || this_font.name;
--
        t_blob := dbms_lob.substr( p_font, t_tables( 'loca' ).length, t_tables( 'loca' ).offset );
        declare
          t_size pls_integer := 2 + this_font.indexToLocFormat * 2; -- 0 for short offsets, 1 for long
        begin
          for i in 0 .. this_font.numGlyphs
          loop
            this_font.loca( i ) := blob2num( t_blob, t_size, 1 + i * t_size );
          end loop;
        end;
      end if;
    end if;
--
    if not this_font.cid
    then
      if this_font.flags = 4 -- a symbolic font
      then
        declare
          t_real pls_integer;
        begin
          for t_code in 32 .. 255
          loop
            t_real := this_font.code2glyph.first + t_code - 32; -- assume code 32, space maps to the first code from the font
            if this_font.code2glyph.exists( t_real )
            then
              this_font.first_char := least( nvl( this_font.first_char, 255 ), t_code );
              this_font.last_char := t_code;
              if this_font.hmetrics.exists( this_font.code2glyph( t_real ) )
              then
                this_font.char_width_tab( t_code ) := trunc( this_font.hmetrics( this_font.code2glyph( t_real ) ) * this_font.unit_norm );
              else
                this_font.char_width_tab( t_code ) := trunc( this_font.hmetrics( this_font.hmetrics.last() ) * this_font.unit_norm );
              end if;
            else
              this_font.char_width_tab( t_code ) := trunc( this_font.hmetrics( 0 ) * this_font.unit_norm );
            end if;
          end loop;
        end;
      else
        declare
          t_unicode pls_integer;
          t_prv_diff pls_integer;
          t_utf16_charset varchar2(1000);
          t_winansi_charset varchar2(1000);
          t_glyphname tp_glyphname;
        begin
          t_prv_diff := -1;
          t_utf16_charset := substr( this_font.charset, 1, instr( this_font.charset, '.' ) ) || 'AL16UTF16';
          t_winansi_charset := substr( this_font.charset, 1, instr( this_font.charset, '.' ) ) || 'WE8MSWIN1252';
          for t_code in 32 .. 255
          loop
            t_unicode := utl_raw.cast_to_binary_integer( utl_raw.convert( hextoraw( to_char( t_code, 'fm0x' ) )
                                                                        , t_utf16_charset
                                                                        , this_font.charset
                                                                        )
                                                       );
            t_glyphname := '';
            this_font.char_width_tab( t_code ) := trunc( this_font.hmetrics( this_font.hmetrics.last() ) * this_font.unit_norm );
            if this_font.code2glyph.exists( t_unicode )
            then
              this_font.first_char := least( nvl( this_font.first_char, 255 ), t_code );
              this_font.last_char := t_code;
              if this_font.hmetrics.exists( this_font.code2glyph( t_unicode ) )
              then
                this_font.char_width_tab( t_code ) := trunc( this_font.hmetrics( this_font.code2glyph( t_unicode ) ) * this_font.unit_norm );
              end if;
              if t_glyph2name.exists( this_font.code2glyph( t_unicode ) )
              then
                if t_glyphnames.exists( t_glyph2name( this_font.code2glyph( t_unicode ) ) )
                then
                  t_glyphname := t_glyphnames( t_glyph2name( this_font.code2glyph( t_unicode ) ) );
                end if;
              end if;
            end if;
--
            if (   t_glyphname is not null
               and t_unicode != utl_raw.cast_to_binary_integer( utl_raw.convert( hextoraw( to_char( t_code, 'fm0x' ) )
                                                                               , t_winansi_charset
                                                                               , this_font.charset
                                                                               )
                                                              )
               )
            then
              this_font.diff := this_font.diff || case when t_prv_diff != t_code - 1 then ' ' || t_code end || ' /' || t_glyphname;
              t_prv_diff := t_code;
            end if;
          end loop;
        end;
        if this_font.diff is not null
        then
          this_font.diff := '/Differences [' || this_font.diff || ']';
        end if;
      end if;
    end if;
--
    t_font_ind := g_fonts.count( ) + 1; 
    g_fonts( t_font_ind ) := this_font;

--
dbms_output.put_line( this_font.fontname || ' ' || this_font.family || ' ' || this_font.style
|| ' ' || this_font.flags
|| ' ' || this_font.code2glyph.first
|| ' ' || this_font.code2glyph.prior( this_font.code2glyph.last )
|| ' ' || this_font.code2glyph.last
|| ' nr glyphs: ' || this_font.numGlyphs
 ); 
--
    return t_font_ind;
  end;
--
  procedure load_ttf_font
    ( p_font blob
    , p_encoding varchar2 := 'WINDOWS-1252'
    , p_embed boolean := false
    , p_compress boolean := true
    , p_offset number := 1
    )
  is
    t_tmp pls_integer;
  begin
    t_tmp := load_ttf_font( p_font, p_encoding, p_embed, p_compress );
  end;
--
/*
  function load_ttf_font
    ( p_dir varchar2 := 'MY_FONTS'
    , p_filename varchar2 := 'BAUHS93.TTF'
    , p_encoding varchar2 := 'WINDOWS-1252'
    , p_embed boolean := false
    , p_compress boolean := true
    )
  return pls_integer
  is
  begin
    return load_ttf_font( file2blob( p_dir, p_filename ), p_encoding, p_embed, p_compress );
  end;
--
  procedure load_ttf_font
    ( p_dir varchar2 := 'MY_FONTS'
    , p_filename varchar2 := 'BAUHS93.TTF'
    , p_encoding varchar2 := 'WINDOWS-1252'
    , p_embed boolean := false
    , p_compress boolean := true
    )
  is
  begin
    load_ttf_font( file2blob( p_dir, p_filename ), p_encoding, p_embed, p_compress );
  end;
--
*/

  procedure load_ttc_fonts
    ( p_ttc blob
    , p_encoding varchar2 := 'WINDOWS-1252'
    , p_embed boolean := false
    , p_compress boolean := true
    )
  is
    type tp_font_table is record
      ( offset pls_integer
      , length pls_integer
      );
    type tp_tables is table of tp_font_table index by varchar2(4);
    t_tables tp_tables;
    t_tag varchar2(4);
    t_blob blob;
    t_offset pls_integer;
    t_font_ind pls_integer;
  begin
    if utl_raw.cast_to_varchar2( dbms_lob.substr( p_ttc, 4, 1 ) ) != 'ttcf'
    then
      return;
    end if;
    for f in 0 .. blob2num( p_ttc, 4, 9 ) - 1
    loop
      t_font_ind := load_ttf_font( p_ttc, p_encoding, p_embed, p_compress, blob2num( p_ttc, 4, 13 + f * 4 ) + 1 );
dbms_output.put_line( t_font_ind || ' ' || g_fonts( t_font_ind ).fontname || ' ' || g_fonts( t_font_ind ).family || ' ' || g_fonts( t_font_ind ).style );
    end loop;
  end;
--
/*
  procedure load_ttc_fonts
    ( p_dir varchar2 := 'MY_FONTS'
    , p_filename varchar2 := 'CAMBRIA.TTC'
    , p_encoding varchar2 := 'WINDOWS-1252'
    , p_embed boolean := false
    , p_compress boolean := true
    )
  is
  begin
    load_ttc_fonts( file2blob( p_dir, p_filename ), p_encoding, p_embed, p_compress );
  end;
*/  
--
  procedure output_font_to_doc( p_output_to_doc boolean )
  is
  begin
    if p_output_to_doc
    then
      p_out( 'BT /F' || g_current_font || ' '
              || to_char_round( g_fonts( g_current_font ).fontsize ) || ' Tf ET');
    end if;
  end;
--  

  procedure set_font
    ( p_index pls_integer
    , p_fontsize_pt number
    , p_output_to_doc boolean := true
    )
  is
  begin
    if p_index is not null
    then
      g_used_fonts( p_index ) := 0;
      g_current_font := p_index;
      g_fonts( p_index ).fontsize := p_fontsize_pt;
      output_font_to_doc( p_output_to_doc );
    end if;

    fontsizePt:=p_fontsize_pt;
    fontsize:=p_fontsize_pt/k;
  end;
--
  function set_font
    ( p_fontname varchar2
    , p_fontsize_pt number
    , p_output_to_doc boolean := true
    )
  return pls_integer
  is
    t_fontname varchar2(100);
  begin
    fontsizePt:=p_fontsize_pt;
    fontsize:=p_fontsize_pt/k;
    if p_fontname is null
    then
      if (  g_current_font is not null
         and p_fontsize_pt != g_fonts( g_current_font ).fontsize
         )
      then
        g_fonts( g_current_font ).fontsize := p_fontsize_pt;
        output_font_to_doc( p_output_to_doc );
      end if;
      return g_current_font;
    end if;
--
    t_fontname := lower( p_fontname );
    for i in g_fonts.first .. g_fonts.last
    loop
      if lower( g_fonts( i ).fontname ) = t_fontname
      then
        exit when g_current_font = i and g_fonts( i ).fontsize = p_fontsize_pt and page is null;
        g_fonts( i ).fontsize := coalesce( p_fontsize_pt
                                         , g_fonts( nvl( g_current_font, i ) ).fontsize
                                         , 12
                                         );
        g_current_font := i;
        g_used_fonts( i ) := 0;
        output_font_to_doc( p_output_to_doc );
        return g_current_font;
      end if;
    end loop;
    return null;
  end;
--
  procedure set_font
    ( p_fontname varchar2
    , p_fontsize_pt number
    , p_output_to_doc boolean := true
    )
  is
    t_dummy pls_integer;
  begin
    t_dummy := set_font( p_fontname, p_fontsize_pt, p_output_to_doc );
  end;
--
  function set_font
    ( p_family varchar2
    , p_style varchar2 := 'N'
    , p_fontsize_pt number := null
    , p_output_to_doc boolean := true
    )
  return pls_integer
  is
    cut_U number; --for underline
    t_family varchar2(100);
    t_style varchar2(100);
  begin
    if p_family is null and g_current_font is null
    then
      return null;
    end if;
    if p_family is null and  p_style is null and p_fontsize_pt is null
    then
      return null;
    end if;
    t_family := coalesce( lower( p_family )
                        , g_fonts( g_current_font ).family
                        );
    t_style := upper( p_style );
    t_style := case t_style
                 when 'NORMAL' then 'N'
                 when 'REGULAR' then 'N'
                 when 'BOLD' then 'B'
                 when 'ITALIC' then 'I'
                 when 'OBLIQUE' then 'I'
                 else t_style
               end;
    cut_U := instr(t_style,'U');
    if cut_U <> 0 then
        underline:=true;
        t_style:=replace(t_style, 'U','');    
    else
        underline:=false;
    end if;
    t_style := coalesce( t_style
                       , case when g_current_font is null then 'N' else g_fonts( g_current_font ).style end
                       );
--
    for i in g_fonts.first .. g_fonts.last
    loop
      if (   g_fonts( i ).family = t_family
         and g_fonts( i ).style = t_style
         )
      then
        return set_font( g_fonts( i ).fontname, p_fontsize_pt, p_output_to_doc );
      end if;
    end loop;
    return null;
  end;
--
  procedure set_font
    ( p_family varchar2
    , p_style varchar2 := 'N'
    , p_fontsize_pt number := null
    , p_output_to_doc boolean := true
    )
  is
    t_dummy pls_integer;
  begin
    t_dummy := set_font( p_family, p_style, p_fontsize_pt, p_output_to_doc );
  end;  

procedure fontRestore is
begin

  set_Font(fontFamilySave, fontStyleSave, fontsizePtSave);

end;


----------------------------------------------------------------------------------------
function GetStringWidth(pstr varchar2) return number is
begin

    
    if pstr is null then
      return 0;
  end if;
  
    return str_len(pstr) * fontsize/1000;
end GetStringWidth;


----------------------------------------------------------------------------------------
procedure SetFontSize(psize number) is
p_output_to_doc boolean := true;
begin

      if (  g_current_font is not null
         and psize != g_fonts( g_current_font ).fontsize
         )
      then
          if(page>0) then
            g_fonts( g_current_font ).fontsize := psize;
            output_font_to_doc( p_output_to_doc );
            fontsizePt:=psize;
            fontsize:=psize/k;
            end if;
      end if;
end SetFontSize;

----------------------------------------------------------------------------------------
procedure Cell       (pw number,
                      ph number default 0,
                      ptxt varchar2 default '',
                      pborder varchar2 default '0',
                      pln number default 0,
                      palign varchar2 default '',
                      pfill number default 0,
                      plink varchar2 default '') is
 myPW number := pw;
 myK k%type := k;
 myX x%type := x;
 myY y%type := y;
 myWS ws%type := ws;
 myS txt;
 myOP txt;
 myDX number;
 begin
  null;
    -- Output a cell
    if( ( y + ph > pageBreakTrigger) and  not InFooter and AcceptPageBreak()) then
        -- Automatic page break
        if(myWS > 0) then
            ws:=0;
            p_out('0 Tw');
        end if;
        AddPage(CurOrientation);
        x:=myX;
        if(myWS > 0) then
            ws := myWS;
            p_out(tochar(myWS * myK,3) ||' Tw');
        end if;
    end if;

    if(myPW = 0) then
        myPW := w - rMargin - x;
    end if;
    myS := '';
    if(pfill = 1 or pborder = '1') then
        if(pfill = 1) then
          if (pborder = '1') then
            myOP :=  'B';
          else
            myOP := 'f';
          end if;
        else
            myOP := 'S';
        end if;
        myS := tochar(x*myK,2)||' '||tochar((h-y)*myK,2)||' '||tochar(myPW*myK,2)||' '||tochar(-ph*myK,2)||' re '||myOP||' ';
    end if;

    if(is_string(pborder)) then
        myX := x;
        myY := y;
        if(instr(pborder,'L') > 0) then
            myS := myS || tochar(myX*myK,2) ||' '||tochar((h-myY)*myK,2)||' m '||tochar(myX*myK,2)||' '||tochar((h-(myY+ph))*myK,2)||' l S ';
        end if;
        if(instr(pborder,'T') > 0) then
            myS := myS || tochar(myX*myK,2)||' '||tochar((h-myY)*myK,2)||' m '||tochar((myX+myPW)*myK,2)||' '||tochar((h-myY)*myK,2)||' l S ';
        end if;
        if(instr(pborder,'R') > 0) then
            myS := myS || tochar((myX+myPW)*myK,2)||' '||tochar((h-myY)*myK,2)||' m '||tochar((myX+myPW)*myK,2)||' '||tochar((h-(myY+ph))*myK,2)||' l S ';
        end if;
        if(instr(pborder,'B') > 0) then
            myS := myS || tochar(myX*myK,2)||' '||tochar((h-(myY+ph))*myK,2)||' m '||tochar((myX+myPW)*myK,2)||' '||tochar((h-(myY+ph))*myK,2)||' l S ';
        end if;
    end if;
    if ptxt is not null then
        
    if(palign='R') then
            myDX := myPW - cMargin - GetStringWidth(ptxt);
        elsif(palign='C') then
            myDX := (myPW - GetStringWidth(ptxt))/2;
        else
            myDX := cMargin;
        end if;
        
    if(ColorFlag) then
            myS := myS || 'q ' || TextColor || ' ';
      end if;
        myS := myS || 'BT '||tochar((x+myDX)*myK,2)||' '||tochar((h-(y+.5*ph+.3*fontsize))*myK,2)||' Td '||txt2raw(ptxt)||' Tj ET';


        if(underline) then
            myS := myS || ' ' || p_dounderline(x+myDX,y+.5*ph+.3*fontsize,ptxt);
        end if;
        if(ColorFlag) then
            myS := myS || ' Q';
        end if;
        if(not empty(plink)) then
            Link(x + myDX,y + .5*ph - .5*fontsize, GetStringWidth(ptxt), fontsize, plink);
        end if;
    end if;
    if(not empty(myS)) then
        p_out(myS);
    end if;

    lasth := ph;


    if( pln>0 ) then
        -- Go to next line
        y := y + ph;
        if(pln=1) then
            x := lMargin;
        end if;
    else
        x := x + myPW;
    end if;

end Cell;

----------------------------------------------------------------------------------------
-- MultiCell : Output text with automatic or explicit line breaks
-- param phMax : give the max height for the multicell. (0 if non applicable)
-- if ph is null : the minimum height is the value of the property LineSpacing
----------------------------------------------------------------------------------------
procedure MultiCell( 
                      pw             number,
                      ph             number         default 0,
                      ptxt         varchar2,
                      pborder    varchar2     default '0',
                      palign     varchar2     default 'J',
                      pfill     number         default 0,
                      phMax     number         default 0
                   ) is

  myPW number := pw;
  myBorder word := pborder;
  myS txt;
  myNB number;
  wmax number;
  myB txt;
  myB2 txt;
  sep number := -1;
--  i number := 0;
--  j number := 0;
  i number := 1;
  j number := 1;
  l number := 0;
  ns number := 0;
  nl number := 1;
  carac word;
  lb_skip boolean := false;
  ls number;
  cumulativeHeight number := 0;
  myH number := pH;
begin
    -- Output text with automatic or explicit line breaks

    -- see if we need to set Height to the minimum linespace
    if (myH = 0) then
      myH := getLineSpacing;
    end if;

    if(myPW = 0) then
        myPW:=w - rMargin - x;
    end if;
    wmax := (myPW - 2 * cMargin) * 1000 / fontsize;
    myS := str_replace(CHR(13),'',ptxt);
    myNB := strlen(myS);
    if(myNB > 0 and substr(myS,-1) = CHR(10) ) then
        myNB := myNB - 1;
    end if;
    myB := 0;

    if (myBorder is not null) then
        if(myBorder = '1') then
            myBorder :='LTRB';
            myB := 'LRT';
            myB2 := 'LR';
        else
            myB2 := '';
            if(instr(myBorder,'L') > 0) then
                myB2 := myB2 || 'L';
            end if;
            if(instr(myBorder,'R') > 0) then
                myB2 := myB2 || 'R';
            end if;
            if (instr(myBorder,'T') > 0) then
              myB := myB2 || 'T';
            else
              myB := myB2;
            end if;
        end if;
    end if;

    while(i <= myNB)
    loop
        lb_skip := false;
        -- Get next character
        carac := substr(myS,i,1);
        if(carac = CHR(10)) then
            -- Explicit line break
            if(ws > 0) then
                ws := 0;
                p_out('0 Tw');
            end if;
            Cell(myPW,myH,substr(myS,j,i-j),myB,2,palign,pfill);
            cumulativeHeight := cumulativeHeight + myH;
            i := i + 1;
            sep := -1;
            j := i;
            l := 0;
            ns := 0;
            nl := nl + 1;
            if(myBorder is not null and nl = 2) then
                myB := myB2;
            end if;
            lb_skip := true;
        end if;

        if (not lb_skip) then
            if(carac =' ') then
                sep := i;
                ls := l;
                ns := ns + 1;
            end if;
            l := l + str_len(carac);
            if( l > wmax) then
                -- Automatic line break
                if(sep=-1) then
                    if(i=j) then
                        i := i + 1;
                    end if;
                    if(ws > 0) then
                        ws := 0;
                        p_out('0 Tw');
                    end if;
                    Cell(myPW,myH,substr(myS,j,i-j),myB,2,palign,pfill);
                else
                    if(palign = 'J') then
                        if (ns > 1) then
                          ws := (wmax - ls)/1000*fontsize/(ns-1);
                        else
                          ws := 0;
                        end if;
                        p_out(''|| tochar(ws*k,3) ||' Tw');
                    end if;
                    Cell(myPW,myH,substr(myS,j,sep-j),myB,2,palign,pfill);
                    i := sep + 1;
                end if;
                cumulativeHeight := cumulativeHeight + myH;
                sep := -1;
                j := i;
                l := 0;
                ns := 0;
                nl := nl + 1;
                if(myBorder is not null and nl = 2) then
                    myB := myB2;
                end if;
            else
              i := i + 1;
            end if;
        end if;
    end loop;

    -- Last chunk
    if(ws > 0) then
        ws := 0;
        p_out('0 Tw');
    end if;

    if(myBorder is not null and instr(myBorder,'B') > 0) then
      if (phMax > 0) then
        if (cumulativeHeight >= phMax) then
          myB := myB || 'B';
        end if;
      else
        myB := myB || 'B';
      end if;
    end if;
    Cell(myPW,myH,substr(myS,j,i-j),myB,2,palign,pfill);
    cumulativeHeight := cumulativeHeight + myH;

    -- add an empty cell if phMax is not reached.
    if (phMax > 0) then
        if ( cumulativeHeight < phMax ) then
            -- dealing with the bottom border.
            if(myBorder is not null and instr(myBorder,'B') > 0) then
                myB := myB || 'B';
            end if;
            Cell(myPW,phMax-cumulativeHeight,null,myB,2,palign,pfill);
        end if;
    end if;

    x := lMargin;

end MultiCell;


procedure image ( imageBlob         blob,
                    px                          number,
                    py                         number,
                    p_width             number default 0,
                    p_height             number default 0,
                  pLink                 varchar2 default null
                      ) is

   myW number := p_width;
   myH number := p_height;
   w_blob blob;

   t_adler32     varchar2( 8 );
     img                 tp_img;

begin

  
  t_adler32 := adler32( imageBlob );

  if (images.exists(t_adler32)) then
        img             := images(t_adler32);
    else
      img                             := parse_img(imageBlob, t_adler32);
        img.i                         := nvl(images.count, 0) + 1;
        images(t_adler32) := img;
    end if;



  if myW = 0 and myH = 0 then
    myW := img.width;
    myH := img.height;
  elsif myH = 0 then
    myH := img.height*(myW/img.width);
  elsif myW = 0 then
    myW := img.width*(myH/img.height);
  end if;


  if DefOrientation = 'P' then
      p_out('q '||tochar(myW, 2)||' 0 0 '||tochar(myH, 2)||' '||tochar(px, 2)||' '||tochar( fhPt-myH - py , 2)||' cm /I'||to_char(img.i)||' Do Q');
  else
    p_out('q '||tochar(myW, 2)||' 0 0 '||tochar(myH, 2)||' '||tochar(px, 2)||' '||tochar( fwPt-myH - py , 2)||' cm /I'||to_char(img.i)||' Do Q');
  end if;
  

end image;


function base64clob_to_blob (p_clob_in in clob) return blob is

  v_blob blob;
  v_offset integer;
  v_buffer_varchar varchar2(32000);
  v_buffer_raw raw(32000);
  v_buffer_size binary_integer := 32000;
begin
--
  if p_clob_in is null then
    return null;
  end if;
-- 
  dbms_lob.createtemporary(v_blob, true);
  v_offset := 1;
  for i in 1..ceil(dbms_lob.getlength(p_clob_in) / v_buffer_size)
  loop
    dbms_lob.read(p_clob_in, v_buffer_size, v_offset, v_buffer_varchar);
    v_buffer_raw := utl_encode.base64_decode(utl_raw.cast_to_raw(v_buffer_varchar));
    dbms_lob.writeappend(v_blob, utl_raw.length(v_buffer_raw), v_buffer_raw);
    v_offset := v_offset + v_buffer_size;
  end loop;
  
  return v_blob;

end;


function get_blob_from_base64_string (p_clob clob) return blob is
  l_chunk blob; --Chunks of decoded blob that'll be appended
  l_result blob; --Final blob result to be returned
  l_rawout raw (32767); --Decoded raw data from first pass decode
  l_rawin raw (32767); --Encoded raw data chunk
  l_amt number default 7700;
  --Default length of data to decode
  l_offset number default 1;
  --Default Offset of data to decode
  l_tempvarchar varchar2 (32767);
begin

  dbms_lob.createtemporary (l_result, false, dbms_lob.call);
  dbms_lob.createtemporary (l_chunk, false, dbms_lob.call);
  loop
    dbms_lob.read (p_clob, l_amt, l_offset, l_tempvarchar);
    l_offset := l_amt + l_offset;
    l_rawin := utl_raw.cast_to_raw (l_tempvarchar);
    l_rawout := utl_encode.base64_decode (l_rawin);
    l_chunk := to_blob (l_rawout);
    dbms_lob.append (l_result, l_chunk);
  end loop;

  return l_result;

exception when no_data_found then null;   return l_result;


end;


procedure image ( imageClob         clob,
                    px                          number,
                    py                         number,
                    p_width             number default 0,
                    p_height             number default 0,
                  pLink                 varchar2 default null
                                ) is
    b blob;

begin


  b := base64clob_to_blob(imageclob);      
  image ( b, px, py, p_width, p_height, pLink);

end image;




/*
/* THIS PROCEDURE HANGS UP ........... */
----------------------------------------------------------------------------------------
procedure Write(pH varchar2,ptxt varchar2,plink varchar2 default null) is
   myW number;     -- remaining width from actual position in user units
   myWmax number;  -- remaining cellspace
   s bigtext;
   c word;
   nb pls_integer;
   sep pls_integer;
   i pls_integer;
   j pls_integer;
   l pls_integer;
   lsep pls_integer;
   lastl pls_integer;
   

begin
   
    -- Output text in flowing mode
    myW := w - rMargin - x;
    myWmax := (myW - 2 * cMargin) * 1000 / FontSize;
    s := str_replace(chr(13),'',ptxt);
    nb := strlen(s);
    sep := -1;   -- no blank space encountered, position of last blank
  i := 1;      -- running position
  j := 1;      -- last remembered position , start for next output
    l := 0;      -- string length since last written
  lsep := 0;   -- position of last blank
  lastl := 0;  -- length till that blank
  -- Loop over all characters
    while i <= nb  loop
        -- Get next character
        c := substr(s, i, 1);

    -- Explicit line break
        if(c = chr(10)) then
            Cell(myW, pH, substr(s,j,i-j), 0, 1, '', 0, plink);
      -- positioned at beginning of new line
            i := i + 1;
            sep := -1;
            j := i;
            l := 0;
      myW := w - rMargin - x;
            myWmax := (myW - 2 * cMargin) * 1000 / FontSize;  -- whole line

    else

           if c = ' ' then
             sep := i;
             lsep := 0;
             lastl := l;
          else
             lsep := lsep + str_len(c);
           end if;
            l := l + str_len(c);
            if l > myWmax then
                -- Automatic line break
                if sep = -1 then  -- forced
          Cell(myW, pH, substr(s,j,i-j+1), 0, 1, '', 0, plink);
                    i := i + 1;
          j := i;
          l := 0;
                else  -- wrap at last blank
                    Cell(myW, pH, substr(s,j,sep-j), 0, 1, '', 0, plink);
                    i := sep + 1;
          j := i;
          sep := -1;
          l := lsep-(myWmax-lastl);  -- rest remaining space from previous line
                                     -- WHY ????
                end if;
        myW := w - rMargin - x;
                myWmax := (myW - 2 * cMargin) * 1000 / FontSize;
            else
                i := i + 1;
            end if;
        end if;
    end loop;
    -- Last chunk
    if( i != j ) then
         Cell((l+2*cMargin) / 1000 * FontSize, pH, substr(s,j), 0, 0, '', 0, plink);
  end if;
end write;


function code39Char(c varchar2) return varchar2 is
begin
  
  case  c
    when '0' then return  'nnnwwnwnn';
    when '1' then return  'wnnwnnnnw';
    when '2' then return  'nnwwnnnnw';
    when '3' then return  'wnwwnnnnn';
    when '4' then return  'nnnwwnnnw';
    when '5' then return  'wnnwwnnnn';
    when '6' then return  'nnwwwnnnn';
    when '7' then return  'nnnwnnwnw';
    when '8' then return  'wnnwnnwnn';
    when '9' then return  'nnwwnnwnn';
    when 'A' then return  'wnnnnwnnw';
    when 'B' then return  'nnwnnwnnw';
    when 'C' then return  'wnwnnwnnn';
    when 'D' then return  'nnnnwwnnw';
    when 'E' then return  'wnnnwwnnn';
    when 'F' then return  'nnwnwwnnn';
    when 'G' then return  'nnnnnwwnw';
    when 'H' then return  'wnnnnwwnn';
    when 'I' then return  'nnwnnwwnn';
    when 'J' then return  'nnnnwwwnn';
    when 'K' then return  'wnnnnnnww';
    when 'L' then return  'nnwnnnnww';
    when 'M' then return  'wnwnnnnwn';
    when 'N' then return  'nnnnwnnww';
    when 'O' then return  'wnnnwnnwn'; 
    when 'P' then return  'nnwnwnnwn';
    when 'Q' then return  'nnnnnnwww';
    when 'R' then return  'wnnnnnwwn';
    when 'S' then return  'nnwnnnwwn';
    when 'T' then return  'nnnnwnwwn';
    when 'U' then return  'wwnnnnnnw';
    when 'V' then return  'nwwnnnnnw';
    when 'W' then return  'wwwnnnnnn';
    when 'X' then return  'nwnnwnnnw';
    when 'Y' then return  'wwnnwnnnn';
    when 'Z' then return  'nwwnwnnnn';
    when '-' then return  'nwnnnnwnw';
    when '.' then return  'wwnnnnwnn';
    when ' ' then return  'nwwnnnwnn';
    when '*' then return  'nwnnwnwnn';
    when '$' then return  'nwnwnwnnn';
    when '/' then return  'nwnwnnnwn';
    when '+' then return  'nwnnnwnwn';
    when '%' then return  'nnnwnwnwn';
  end case;
  
  raise no_data_found;
  
end;


procedure code39(xpos number, ypos number, code varchar2, bottomtext varchar2 default null, baseline number default 0.5, height number default 5) is

  x         number  := xpos;

  wide      number  := baseline;
  narrow    number  := baseline / 3; 
  gap       number  := narrow;

  lineWidth number;

  cd        varchar2(50);
  seq       varchar2(10);
 
  xSave number    := getx;
  ySave number    := gety;

begin

  SetFillColor(0);
    
  cd   := '*'||upper(code)||'*';

  for i in 1 .. length(cd) loop

    seq := code39Char( substr(cd, i, 1) );
      
    
    for bar in 0..8 loop

      if substr(seq, bar+1, 1) = 'n' then
        lineWidth := narrow;
      else
        lineWidth := wide;
      end if;
      
      if bar mod 2 = 0 then
        Rect(x, ypos, lineWidth, height, 'F');
      end if;
      
      x := x + lineWidth;
    
    end loop;
    
    x := x + gap;
    
  end loop;

  if bottomtext = 'smurf' then    
    Set_Font('Helvetica', 'N', 6);
    SetFillColor(255,255,255);
    setxy( xpos +((x - xpos) - GetStringWidth(code)-1) / 2, ypos+height-1);
    cell(GetStringWidth(code)+2, 2.5, code, 0,  0, 'L', 1);
  end if; 

  if bottomtext = 'below' then        
    Set_Font('Helvetica', 'N', 6);
    Text( xpos +((x - xpos) - GetStringWidth(code)) / 2, ypos + height +2, code);    
  end if; 

  setxy(xSave, ySave);
  
end;


----------------------------------------------------------------------------------------
procedure Output(pname varchar2 default null,pdest varchar2 default null) is
   myName word := pname;
   myDest word := pdest;
   w_blob blob;

begin

 
     -- Output PDF to some destination
     -- Finish document if necessary

   if state < 3 then
        ClosePDF();
   end if;
   
     myDest := strtoupper(myDest);

     if(myDest is null) then
          if(myName is null) then
               myName := 'doc.pdf';
               myDest := 'I';
          else
               myDest := 'D';
          end if;
     end if;

  if pname = 'debug' then
    myDest := 'debug';
  end if;

     
          
     if (myDest = 'I') then
          -- Send as pdf to a browser
          htp.init;
          owa_util.mime_header('application/pdf',false);
          htp.print('Content-Length: ' || getPDFDocLength());
          htp.print('Content-disposition: inline; filename="' || myName || '"');
          owa_util.http_header_close;
          
          wpg_docload.download_file(pdfBoc);

     elsif (myDest = 'D') then

            htp.init;
      -- Download file
            if(not empty(owa_util.get_cgi_env('HTTP_USER_AGENT')) and instr(owa_util.get_cgi_env('HTTP_USER_AGENT'),'MSIE') > 0) then
                OWA_UTIL.MIME_HEADER('application/force-download',false);
            else
                OWA_UTIL.MIME_HEADER('application/octet-stream',false);
            end if;
            htp.print('Content-Length: ' || getPDFDocLength());
            htp.print('Content-disposition: attachment; filename="' || myName || '"');
            owa_util.http_header_close;
                
            wpg_docload.download_file(pdfBoc);        

     elsif (myDest = 'S') then
            htp.init;
            owa_util.mime_header('text/html');
            htpc(replace(replace(replace(clobfromblob(pdfBoc),'<', ';'),'>',';'),chr(10),'<br/>'));
            

     elsif myDest = 'debug' then
            htpc('<pre>'||replace(replace(replace(clobfromblob(pdfBoc),'<', ';'),'>',';'),chr(10),'<br/>')||'</pre>');

     else
            raise_application_error(-20100,'Incorrect output destination: ' || myDest);
     end if;
    
end Output;


function Output return blob is
begin

     if state < 3 then
          ClosePDF();
     end if;

    return pdfBoc;          
    
end Output;


procedure thDraw is

  cellWidth   number;
  hx_before   number;
  hx_after    number;
  
  hy          number;
  
  bdr         varchar2(1);

begin

  if inTh then return; end if;


  if headers.count > 0 and not headers(1).hideheader then
    
    for i in 1..headers.count loop
  
      set_Font(headers(i).hdr_fontFamily, headers(i).hdr_fontStyle, tonumber(headers(i).hdr_fontSize));


      if thUnderLine or  headers(1).hdr_ul then
        bdr := 'B';
      else
        bdr := '0';
      end if;
      

      if headers(i).fox is not null then

        cellWidth := GetStringWidth(headers(i).label_n);
        hx_before := getx;
        
        cell(headers(i).width, 5, headers(i).label_n, bdr, 0, headers(i).hdr_align);
        
        hx_after := getx;
        hy       := gety;
          
        setxy(hx_before + cellWidth + 1.3, y -.35);
    
    
        dbms_output.put_line('Uporabljam tdDraw');
        set_Font(headers(i).hdr_fontFamily,'I', tonumber(headers(i).hdr_fontSize)-2);
        cell(10, 5, headers(i).fox, 0, 0, 'L');
        setxy(hx_after,hy);
            
      else
        cell(headers(i).width, 5, headers(i).label_n, bdr, 0,  headers(i).hdr_align);
      end if;



    end loop;

    ln(5);

  end if;

end;


----------------------------------------------------------------------------------------
-- MyRepetitiveHeader :  Proc that illustrates Header hooks.
-- The Header hook procedure has to be PUBLIC. (Spec declaration needeed).
----------------------------------------------------------------------------------------
procedure MyRepetitiveHeader(param1 varchar2, param2 varchar2) is
begin
  
  Set_Font('Helvetica','B',10);
    SetFillColor(240);
  
  cell(0,9, param1, 1, 0, 'C', 1);

  Set_Font('Helvetica','B',8);
  cell(0,9, param2||'   Page: '||PageNo || ' of `p  ' , 1, 1, 'R', 1);

  ln(7);
    
  --thDraw;
  
     -- line(1,1.2,20,1.2);
end MyRepetitiveHeader;

----------------------------------------------------------------------------------------
-- MyRepetitiveFooter :  Proc that illustrates Footer hooks.
-- The Footer hook procedure has to be PUBLIC. (Spec declaration needeed).
----------------------------------------------------------------------------------------
procedure MyRepetitiveFooter(param1 varchar2) is
begin

  fontSave;
  Set_Font('Helvetica','N',9);
  setxy(10, GetPageHeight-4); 
  cell(0, 6, 'Footer !!!! - Keep confidential. Handle with Due Care.',  0, 0, 'C', 0);
  fontRestore;

end MyRepetitiveFooter;

----------------------------------------------------------------------------------------
-- testHeader :  Proc that illustrates how to call header and footer hooks.
----------------------------------------------------------------------------------------
procedure testHeader is
 img varchar2(2000);
 tHdr tv4000a; -- This is a table for the custom header proc hooked
 tFtr tv4000a; -- This is a table for the custom header proc hooked
begin
    -- this function has never been tested --- 6/1/2012

    -- setting parameter values for proc 'MyRepetitiveHeader'
    tHdr('param1') := 'Value for Param1';
    tHdr('param2') := '123456';

    setHeaderProc('gen_pdf.MyRepetitiveHeader', tHdr);

    tFtr('param1') := 'dummy';

    setFooterProc('gen_pdf.MyRepetitiveFooter',tFtr);
    pdf('P','cm','A4');
    openpdf;
    -- first page
    AddPage();
    Set_Font('Helvetica','B',16);
    Cell(0,1.2,'This is the first Page (left alignment)',0,1,'L');
    -- Second page
    AddPage();
    Set_Font('Helvetica','B',16);
    Cell(0,1.2,'This is the second Page (right alignment)',0,1,'R');
    -- third page
    AddPage();
    Set_Font('Helvetica','B',16);
    Cell(0,1.2,'This is the 3. Page (centered)',0,1,'C');
    Output();
end testHeader;


function getCenterX( s varchar2) return number is
begin
    return (w/2) - (GetStringWidth('RETURN STATEMENT')/2);
end;


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
                     ) is

    sy                     number;
  sx                     number;
  sx2                    number;
  
    rm                     number;

  lmaxHeight    number;
  lMaxWidth    number;
  rMaxWidth        number;

  tHdr                 tv4000a;
    
  l1a  varchar2(32727);
  l1b  varchar2(32727);
  
  l2a  varchar2(32727);
  l2b  varchar2(32727);

  l3a  varchar2(32727);
  l3b  varchar2(32727);

  l4a  varchar2(32727);
  l4b  varchar2(32727);
  
begin

  pdf('P','mm','letter');

    AliasNbPages    := '`p';

    SetMargins(10,20,10);
  openpdf;
  SetAutoPageBreak(true);

  AddPage();

    sx := getx();
    sy := gety();


  if barcode is not null then
    code39(bc_x, bc_y, barcode, bc_label);
  end if;

    Set_Font('Helvetica','B',10);
  cell(0,4, store_name ,0,1,'l');

  Set_Font('Helvetica','B',8);  
  cell(0,4, store_addr_1 ,0,1,'l');
  
  if store_addr_2 is not null then
    cell(0,4, store_addr_2 ,0,1,'l');
  end if;
  
  cell(0,4, store_city||', '||store_state||' '||store_zip ,0,1,'l');
  cell(0,4, 'Phone: '||nvl_nbsp(store_phone) ,0,1,'l');
  
  ln(3.5);


  if instr(l1,'^') > 0 then

    l1a  := posOf(l1,1,'^');
    l1b  := posOf(l1,2,'^');    
    l2a  := posOf(l2,1,'^');
    l2b  := posOf(l2,2,'^');
    l3a  := posOf(l3,1,'^');
    l3b  := posOf(l3,2,'^');
    l4a  := posOf(l4,1,'^');
    l4b  := posOf(l4,2,'^');


    lMaxWidth := round(greatest(GetStringWidth(l1a),GetStringWidth(l2a), GetStringWidth(l3a), GetStringWidth(l4a)) +1);

    if trim(l1a) is not null or trim(l1b) is not null then
      set_Font('Helvetica','B',8);
      cell(lMaxWidth, 4, l1a ,0,0,'l');
      set_Font('Helvetica','',8);
      cell(90,        4, l1b ,0,1,'l');
    end if;
    
    if trim(l2a) is not null or trim(l2b) is not null then
      set_Font('Helvetica','B',8);
      cell(lMaxWidth, 4, l2a ,0,0,'l');
      set_Font('Helvetica','',8);
      cell(30,        4, l2b ,0,1,'l');
    end if;

    if trim(l3a) is not null or trim(l3b) is not null then
      set_Font('Helvetica','B',8);
      cell(lMaxWidth, 4, l3a ,0,0,'l');
      set_Font('Helvetica','',8);
      cell(30,        4, l3b ,0,1,'l');
    end if;

    if trim(l4a) is not null or trim(l4b) is not null then
      set_Font('Helvetica','B',8);
      cell(lMaxWidth, 4, l4a ,0,0,'l');
      set_Font('Helvetica','',8);
      cell(30,        4, l4b ,0,1,'l');
    end if;
    
  else
    Set_Font('Helvetical','',8);
    if trim(l1) is not null then    Cell(0,3.5, trim(l1), 0, 1,'L');    end if;
    if trim(l2) is not null then     Cell(0,3.5, trim(l2), 0, 1,'L');    end if;
    if trim(l3) is not null then     Cell(0,3.5, trim(l3), 0, 1,'L');    end if;
    if trim(l4) is not null then     Cell(0,3.5, trim(l4), 0, 1,'L');    end if;
  end if;
  
    lMaxHeight := gety;
  
    
  setxy(  ( 2*w/3 )  , sy-1.6);
  --setxy(  ( 2*w/3 )  , sy);
  
  Set_Font('Helvetica','B',12.5);
  Cell(0, 8, t ,0,1,'R');

    --rm := gety;  
  --ln(9);

  Set_Font('Helvetica','N',8);

    SetFillColor(240);
  
  rMaxWidth := greatest (round(GetStringWidth(nbr)) +3, 25);
  setxy(  (w - sx - (rMaxWidth + 50)), y);    
    sx2     := getx;
  
  Cell(rMaxWidth, 4, nbrLabel,     1, 0, 'C', 1);
    Cell(25,                 4, dtLabel,         1, 0, 'C', 1);
  Cell(25,                 4, 'Page',            1, 1, 'C', 1);

    setxy( sx2, y);    

  Cell(rMaxWidth, 4, nbr,        1, 0, 'C', 0);
    Cell(25,                 4, dt,         1, 0, 'C', 0);
    Cell(25,                 4, pageNo || ' of `p',    1, 1, 'C', 0);

  ln(3.5);

    if trim(r1) is not null then 
    setxy(  sx2, y);  
    if substr(r1,1,1) = '^' then
      set_Font('Helvetica',posOf(r1,2,'^'),8);
      Cell(w/3-sx, 3.5, posOf(r1||'^',3,'^') ,0,1,'L');
      Set_Font('Helvetica','N',8);
    else      
      Cell(w/3-sx, 3.5, trim(r1) ,0,1,'L');
    end if;
  end if;    
    if trim(r2) is not null then setxy(  sx2, y);    Cell(w/3-sx, 3.5,    trim(r2) ,0,1,'L'); end if;
    if trim(r3) is not null then setxy(  sx2, y);    Cell(w/3-sx, 3.5, trim(r3) ,0,1,'L'); end if;
    if trim(r4) is not null then setxy(  sx2, y);    Cell(w/3-sx, 3.5, trim(r4) ,0,1,'L'); end if;
    if trim(r5) is not null then setxy(  sx2, y);    Cell(w/3-sx, 3.5, trim(r5) ,0,1,'L'); end if;


  if gety < lMaxHeight then
      sety(lMaxHeight);
  end if;

  ln(8);

    tHdr('param1') := store_name;
  tHdr('param2') := t;

  setHeaderProc('gen_pdf.MyRepetitiveHeader', tHdr);
  --setFooterProc('wpdf.MyRepetitiveFooter', thdr);

end;


procedure headerLandscape(
                            header_1  varchar2,
                            header_2  varchar2 default null,
                            header_3  varchar2 default null,
                            header_4  varchar2 default null 
                          ) is
  
begin

  
  pdf('L','mm','A4');
  aliasNbPages  := '`p';
  setMargins(9,6,9);

  openPdf;
  setAutoPageBreak(true);

  AddPage();
  Set_Font('Helvetica','B',10);     Cell(0,5, header_1, 0,1,'C');

  if header_2 is not null then 
    Set_Font('Helvetica','B',9);    Cell(0,4, header_2, 0,1,'C');
  end if;
    
  if header_3 is not null then 
    Set_Font('Helvetica','B',8);    Cell(0,4, header_3, 0,1,'C');
  end if;

  if header_4 is not null then 
    Set_Font('Helvetica','B',8);    Cell(0,4, header_4, 0,1,'C');
  end if;


  ln(8);

  --tHdr('param1') := store_name;
  --tHdr('param2') := t;

  --setHeaderProc('wpdf.MyRepetitiveHeader', tHdr);
  -- setFooterProc('wpdf.MyRepetitiveFooter');

end;



procedure Arc(x1 IN number, y1 IN number, x2 IN number, y2 IN number, x3 IN number, y3 IN number) IS
begin
        p_out(tochar(x1*k,2)||' '||tochar((h-y1)*k,2)||' '||tochar(x2*k,2)||' '||tochar((h-y2)*k,2)||' '||tochar(x3*k,2)||' '||tochar((h-y3)*k,2)||' c ');
end;

procedure RoundedRect(x IN number, y IN number, w IN number, he IN number, r IN number, corners IN varchar2, style IN varchar2) IS
 xc            number := h;
 yc            number := h;
 
 op         varchar2(1);
 MyArc    number;    
 
begin

  if style = 'F' then
        op := 'f';
  elsif style = 'FD' or style = 'DF' then
      op := 'B';
  else
      op := 'S';
  end if;
  
  MyArc := 4/3 * (sqrt(2) - 1);
 
  p_out( tochar((x+r)*k,2) || ' ' || tochar( (h-y)*k,2) || ' m');
  
  xc := x+w-r;
  yc := y+r;
       
  p_out( tochar(xc*k,2) || ' ' || tochar( (h-y)*k,2) || ' l');
   if (instr(corners, '2')=0) then
      p_out( tochar((x+w)*k,2) || ' ' || tochar( (h-y)*k,2) || ' l');
   else
       Arc(xc + r*MyArc, yc - r, xc + r, yc - r*MyArc, xc + r, yc);
   end if;
   
   xc := x+w-r;
   yc := y+he-r;
   p_out(tochar((x+w)*k,2) || ' ' || tochar( (h-yc)*k,2) || ' l');
   if (instr(corners, '3')=0) then
        p_out(tochar((x+w)*k,2) || ' ' || tochar( (h-(y+he))*k,2) || ' l' );
   else
      Arc(xc + r, yc + r*MyArc, xc + r*MyArc, yc + r, xc, yc + r);
   end if;

   xc := x+r;
   yc := y+he-r;
   p_out(tochar(xc*k,2) || ' ' || tochar( (h-(y+he))*k,2) || ' l');
   if (instr(corners, '4')=0) then
       p_out(tochar(x*k,2) || ' ' || tochar( (h-(y+he))*k,2) || ' l');
   else
       Arc(xc - r*MyArc, yc + r, xc - r, yc + r*MyArc, xc - r, yc);
   end if;
    xc := x+r ;
    yc := y+r;
    p_out(tochar(x*k,2) || ' ' || tochar( (h-yc)*k,2) || ' l');
    if (instr(corners, '1')=0) then
        p_out(tochar(x*k,2) || ' ' || tochar( (h-y)*k,2) || ' l');
        p_out(tochar((x+r)*k,2) || ' ' || tochar( (h-y)*k,2) || ' l');
    else
        Arc(xc - r, yc - r*MyArc, xc - r*MyArc, yc - r, xc, yc - r);
    end if;
    p_out(op);
end;




function NbLines(win number, txt varchar2) return number is

    wmy        number := win;
    wMax     number ;
    nb        number;
    s         varchar2(32000);

    sep number     := -1;
--  i number         := 0;
--  j number         := 0;
  i number         := 1;
  j number         := 1;
  l number         := 0;
  ns number     := 0;
  nl number     := 1;
  
  c     word;

begin


    if wmy = 0 then
        wmy := wmy - rMargin - x;
    end if;

  wmax     := (wmy-2*cMargin)*1000/FontSize;
     s            := str_replace(CHR(13),'',txt);
  nb        := strlen(s);

    if(nb > 0 and substr(s,-1) = CHR(10) ) then
        nb := nb - 1;
    end if;

    while(i <= nb) loop
        c := substr(s,i,1);
        if(c = CHR(10)) then
            i := i + 1;
            sep := -1;
            j := i;
            l := 0;
            nl := nl + 1;
            goto endofloop;
    end if;
    
    if(c = ' ') then
      sep := i;
        end if;
    
        l := l + str_len(c);

        if l > wmax then
            if sep = -1 then
        if i = j then
          i := i + 1;
        end if;
            else
          i := sep + 1;
            end if;

      sep := -1;
      j := i;
      l := 0;
      nl := nl + 1;
        else
          i := i + 1;
        end if;
  
        <<endofloop>>
    null;
  end loop;     

    return nl;
  
end;


procedure CheckPageBreak(h number) is
begin

    if GetY()+h > PageBreakTrigger then
       AddPage(CurOrientation);
    end if;
        
end;


function extract(token varchar2, style varchar2) return varchar2 is

    loc pls_integer;
  s        pls_integer;
    e        pls_integer;

begin

  loc := instr(style, token);
  if loc = 0 then 
    return null; 
  end if;     

  s := instr(style||':', ':', loc);
  e    := instr(style||';', ';', loc);    

  return substr(style||';', s+1, e-s-1); 

end;



procedure tdLineHeight(h number) is
begin
  tdLineHeightGlobal := h;    
end;

procedure trRowSpace(h number) is
begin
  trRowSpaceGlobal := h;    
end;


procedure thUnder is
begin
  
    thUnderLine := true;
      
end;

procedure tdborder(b boolean) is
begin
  
  if b then
    tdBorderGlobal := 1;
  else
    tdBorderGlobal := 0;
  end if;
      
end;



procedure tdFill(b boolean) is
begin
  
  if b then
    tdFillGlobal := 1;
  else
    tdFillGlobal := 0;
  end if;
      
end;



procedure tEnd is
begin

  headers.delete;
  tdCt              := null;
  thUnderLine       := false;
  tdBorderGlobal    := 0;
  
  tdLineHeightGlobal  := 5.5;
  trRowSpaceGlobal    := 0;
  
end;


procedure tClear is
begin

  tEnd;
  
end;



procedure tClear(ct in out number) is
begin

  ct := 0;
  headers.delete;
  tdCt        := null;
  thUnderLine := false;
  
end;


procedure sh1 ( ct in out number)  is
begin
  if ct is null or ct = 0 then
    ct :=1;
  end if;
  if ct mod 2 = 1 then 
    gen_pdf.SetFillColor(255,255,255);
  else
    gen_pdf.SetFillColor(255,255,102);
  end if;
  ct := ct +1;
end;


procedure sh2 ( ct in out number)  is
begin
  if ct is null or ct = 0 then
    ct :=1;
  end if;
  if ct mod 2 = 1 then 
    gen_pdf.SetFillColor(255,255,102);
  else
    gen_pdf.SetFillColor(255,255,255);
  end if;
  ct := ct +1;
end;


procedure sh ( ct in out number)  is
begin
  
  if tdBorderGlobal = '1' and tdFillGlobal = 1 then
    sh1(ct);
  else
    sh2(ct);
  end if;
 

end;


procedure th(
              width       number, 
              label       varchar2, 
              align       varchar2 default 'c', 
              hideHeader  boolean  default false, 
              label_n     varchar2 default null, 
              fox         varchar2 default null,
              ul          boolean  default true
            ) is

    ct number;

begin

  inTh                    := true;

    ct                                       := headers.count +1;
  headers(ct).x           := getx();  
  headers(ct).y           := gety();  
  headers(ct).width       := width;    
    headers(ct).label       := label;    
  headers(ct).label_n     := nvl(label_n, label);  
  headers(ct).fox         := fox;  
  headers(ct).hideheader  := hideHeader;  


  headers(ct).hdr_fontFamily  := fontFamily;
  headers(ct).hdr_fontStyle   := fontstyle;
  headers(ct).hdr_fontSize    := fontsizePt;
  headers(ct).hdr_align       := upper(align);
  headers(ct).hdr_ul          := ul;


  if ct = 1 and GetY() + (fontsizePt *2) > PageBreakTrigger then
    CheckPageBreak( (fontsizePt *2)  );
  end if;


  if not headers(1).hideheader then
    if thUnderLine or headers(1).hdr_ul then
        cell(width,5,label, 'B',0,upper(align));
    else
      cell(width,5,label, 0,0,upper(align));
    end if;
  end if;
  
end;



procedure td(style varchar2, d varchar2) is

    fontFamily        varchar2(15)    := 'Helvetica';
  fontSize      varchar2(10)  := '8';
  fontStyle     varchar2(10)  := '';

  fontWeight    varchar2(15)  := '';
  fontUnder     varchar2(15)  := '';
  fontItalic    varchar2(15)  := '';
  
  border        varchar2(10);
  fill          varchar2(10);

  align                     varchar2(10);
  colspan             number;
    colspanWidth     number    := 0;
    lineHeight        number    := tdLineHeightGlobal;

  xStart number;

  x            number;
  y            number;
    
  yAft    number;

    nb                            number;
  nbrLinesMax            number;
  lineHeightMin        number;
    h                                number;

  tdCurrent                number;
    idx                            number;

begin

    if tdCt is null then
      Set_Font('Helvetica','N',8);
        if not headers(1).hideheader then
      ln(5);
    end if;
    inTh  := false;
    tdCt     := 0;    
    setx(headers(1).x);
  end if;

    if style is not null then
  
    align             := extract('align', style);
    if align is not null then 
        if align = 'left' then
            align := 'l';
        elsif align = 'right' then
          align := 'r';
      elsif align = 'center' then
          align := 'c';
      else 
          align := 'l'; --null;
      end if;
        end if;
    
      fontFamily     := nvl(extract('font-family', style),'Helvetica');
      fontSize         := nvl(extract('font-size', style), 9);
      fontWeight     := extract('font-weight', style);

    if fontWeight = 'bold' then
      fontStyle  := 'B';
    end if;

    fontUnder   := extract('text-decoration', style);
    if fontUnder = 'underline' then
      fontStyle  := fontStyle ||'U';
    end if;

    fontItalic  := extract('font-style', style);
    if fontItalic = 'italic' then
      fontStyle  := fontStyle ||'I';
    end if;
    
    if fontStyle is null OR fontStyle = '' then
     fontStyle := 'N';
    end if;

      colspan         := extract('colspan', style);
      lineHeight     := nvl(extract('line-height', style),tdLineHeightGlobal);
    border      := extract('border', style);
    fill :=  extract('fill', style);
  
  end if;
       
    tdCt             := tdCt +1;
  tdCurrent    := tdCt;
   
  if colspan is not null then
      if colspan > headers.count - (tdCt -1) then
          colspan := headers.count - (tdCt -1);
      end if;
      for i in (tdCt)..(tdCt -1 + colspan) loop
        colspanWidth    := colspanWidth + headers(i).width;
    end loop;        
      tdCt := tdCt + colspan-1;
  else
      colspan                := 1;
      colspanWidth    := headers(tdCt).width;
  end if;

    headers(tdCurrent).fontFamily        := fontFamily;
    headers(tdCurrent).fontSize            := fontSize;
    headers(tdCurrent).fontStyle        := fontStyle;
    headers(tdCurrent).colspan            := colspan;
    headers(tdCurrent).colspanwidth := colspanWidth;
    headers(tdCurrent).align                := upper(align);
    headers(tdCurrent).lineheight        := lineheight;
   headers(tdCurrent).border       := nvl(border, tdBorderGlobal);
   headers(tdCurrent).fill       := nvl(fill, tdFillGlobal);
   headers(tdCurrent).data                    := trim(d);  

  if tdCt >= headers.count then
        nbrLinesMax            := 0;
    lineHeightMin        := 0;
    idx                         := 1;

      while idx <= headers.count loop
        Set_Font(headers(idx).fontFamily, headers(idx).fontStyle, tonumber(headers(idx).fontSize));
        
           
        
      nb := NbLines(headers(idx).colspanWidth, headers(idx).data);
        if nb > nbrLinesMax then
          nbrLinesMax    := nb;
      end if;
      
      if headers(idx).lineHeight > lineHeightMin then
          lineHeightMin    := headers(idx).lineHeight;
      end if;
      
     
      idx := idx + headers(idx).colspan;
      
    end loop;
        
    h    := lineHeightMin * (nbrLinesMax);
    
    CheckPageBreak(h);

    tdSavePos := true;

        idx    := 1;
      while idx <= headers.count loop
        x    := getx;
        y    := gety;

      tdIdCurrent := idx;
     
         Set_Font(headers(idx).fontFamily,headers(idx).fontStyle,tonumber(headers(idx).fontSize));

          multiCell(headers(idx).colspanWidth, lineHeightMin, headers(idx).data, headers(idx).border, headers(idx).align, headers(idx).fill);
        
     
      tdIdxLast             := idx;
      setxy(x + headers(idx).colspanWidth, y);

      idx := idx + headers(idx).colspan;
        end loop;

    tdSavePos := false;

        ln(h + trRowSpaceGlobal);
    setx(headers(1).x);  -- put x to 1st col's in case table is moved right
    
    tdCt         := 0;
  end if;
    
end;



function get_emps_chart return varchar2 is
  t    VARCHAR2(30);
  u    NUMBER;
  chd  VARCHAR2(4000);
  chd2 VARCHAR2(4000);
  chxl1 VARCHAR2(4000);
  chxl2 VARCHAR2(4000);
  chxl VARCHAR2(4000);
  chl  VARCHAR2(4000);
  CURSOR c
  IS
  select e.ename tsname, round((1-(e.sal/maks.sal))*100) as used_pct, round((e.sal/maks.sal*100)) as free_pct from
  (select MAX(SAL) sal from emp) maks,
  (select ENAME, sal  from emp) e;
BEGIN
  FOR usage IN c
  LOOP
    IF chd IS NULL THEN
      chd  := usage.used_pct;
    ELSE
      chd := chd || ',' || usage.used_pct;
    END IF;
    IF chd2 IS NULL THEN
      chd2  := usage.free_pct;
    ELSE
      chd2 := chd2 || ',' || usage.free_pct;
    END IF;

    IF chxl1 IS NULL THEN
      chxl1  := usage.tsname;
    ELSE
      chxl1 := chxl1 || '|' || usage.tsname;
    END IF;
    IF chxl2 IS NULL THEN
      chxl2  := usage.used_pct;
    ELSE
      chxl2 := usage.used_pct ||'|'||chxl2;
    END IF;
  END LOOP;
  chd := chd ||'|'|| chd2;
  chxl:='1:|'||chxl1||'|2:|'||chxl2;
  return replace(('http://chart.googleapis.com/chart?chof=png&'||'cht=bhs&'||'chs=400x480&'||'chco=4D89F9,C6D9FD'
    || '&'||'chtt=Emloyers+sal+(%)'
    || '&'||'chxt=x,y,r'
    || '&'||'chl=' || chl || '&'||'chd=t:' || chd || '&'||'chxl=' || chxl),' ','%20');
END;


procedure emp_demo is 
  str varchar2(300);
  w_num number DEFAULT 0;
  t_logo varchar2(32767) := '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';
  x pls_integer;
 tHdr tv4000a; -- This is a table for the custom header proc hooked
 current_job varchar2(100);
 begin
    -- reset all variables, arrays....
    resetwpdf;
    
    -- setting parameter values for proc 'MyRepetitiveHeader'
    tHdr('param1') := 'Employers';
    tHdr('param2') := v('APP_USER')||' - '||to_char(sysdate,'DD.MM.YYYY');
    setHeaderProc('gen_pdf.MyRepetitiveHeader', tHdr);
    
    -- Setting page size/orienation/automatic page change
    pdf('P','mm','letter');
    AliasNbPages    := '`p';
   
    openpdf;
    SetMargins(10,5,5);
    SetTitle('Employers list');
    SetSubject('Employers list test');
    SetAuthor(v('APP_USER'));
    SetCreator(v('APP_USER'));
    SetKeywords('EMP, Employer, SAL');

    AddPage();
    Set_Font('Times','B', 20);
    Cell(0,15,'Employers',5,10,'C');
    ln(5);
     Set_Font('Times','B', 16);
    cell(0,0,'GEN_PDF = WPDF+AS_PDF3',0,0,'R');
    ln(20);
    image(t_logo, 25, 50, 100, 100); 
    --Image(getFileFromDatabase('isa_logo.jpg'), 120, 30, 0, 0, null);
    Set_Font('Times','B',10);

    th(20, 'Emp NO.', 'l');
    th(40, 'Name', 'l');
    --th(40, 'Job', 'l');
    th(20, 'Hiredate');
    th(15, 'Sal', 'r');

   
    current_job:=' ';
    for z in (SELECT ROWNUM, EMPNO, ENAME, JOB, HIREDATE, SAL  FROM EMP ORDER BY JOB) loop
    
        if z.JOB <> current_job then
            SetFillColor(207,202,255);
            tdFill(true);    
            current_job:=z.job;
            td('border:1;font-size:9;colspan:4;font-weight:bold;','JOB :'||z.JOB); 
        end if;
    
             tdFill(false);

        td('font-size:7.5;',z.EMPNO );
        td('font-size:7.5;align:left', z.ENAME);
        --td('font-size:7.5;align:left;', z.JOB);
        td('font-size:7.5;align:center;', to_char(z.HIREDATE,'DD.MM.YYYY'));
        SetFillColor(255,210,118);
        td('border:1;fill:1;font-size:9;align:right;font-weight:bold;',z.SAL );
        
        w_num:=w_num+z.SAL;
    
  end loop;
  SetFillColor(255,255,20);
  tdFill(true);
  td('border:1;colspan:4;align:right;font-weight:bold;', 'SUM:  $'|| w_num );
  tdFill(false);
  dbms_output.put_line( 'Call load_ttf_font....');
   x := load_ttf_font( getFileFromDatabase('refsan.ttf'), 'CID', p_compress => false );
   set_font( x, 12 );
  ln(5);
  ln(5);
   SetTextColor(32,32,255); 
  write(5, 'Added  refsan.ttf  CID : p_compress => false');
    ln(5);
    write(5, 'Test test šđžćč nova vrstica 123 šđžćč nova vrstica 123 šđžćč nova vrstica 123 šđžćč nova vrstica 123 šđžćč nova vrstica 123 šđžćč nova vrstica 123 šđžćč nova vrstica 123 ');
    ln(5);
    write(5, 'Albanian: Kush mund të lexoni këtë diçka si kjo');
    ln(5);
    underline:=true;
    write(5, 'Croatic: Tko može čitćžđĐŠČĆŽati to nešto poput ovoga' );
    underline:=false;
    ln(5);
  Set_Font('Times','B', 8);
  SetTextColor(255,0,0);
  text(15, 45, 'ANDREJGR');ln(5);
  write(5, 'Test new line?');ln(5);
  ln(5);
  Set_Font('Times','BUI', 8);
  write(5, 'Test Underline! ');
   SetTextColor(null,null,null);
  ln(5);
  ln(5);

  tClear;
  Set_Font('Times-Roman',8);
  th(30, 'Col 1', 'l');
  th(15, 'Col 2');

  for z in 1..15 loop    
    td('font-size:6;line-height:3;', 'Row '|| z||
       ' this is a wraping row, as it will wrap. '|| z );
    td('font-size:6;align:center;line-height:3;', 'item '|| z*2);      
  end loop;
  --output('Emp.pdf','D');
  output('Emp.pdf','I');
  --output('debug');
exception  when others then   pdf_error;
end;

procedure fonts_demo is 
  str varchar2(300);
  w_num number DEFAULT 0;
  x pls_integer;
 tHdr tv4000a; -- This is a table for the custom header proc hooked
 t_ind pls_integer;
 begin
    -- reset all variables, arrays....
    resetwpdf;
    
    -- Setting page size/orienation/automatic page change
    pdf('L','mm','letter');
   
    openpdf;
    SetMargins(5,5,5);
    SetTitle('Fonts list');
    SetSubject('Fonts');
    SetAuthor(v('APP_USER'));
    SetCreator(v('APP_USER'));
    SetKeywords('FONT, FONTS');

    AddPage();
    x:= load_ttf_font( getFileFromDatabase('BAUHS93.ttf'), 'CID', p_compress => false );
    set_font( x, 30 );
    Cell(0,15,'Fonts',5,10,'C');
    ln(5);
     Set_Font('Times','B', 16);
    cell(0,0,'GEN_PDF = WPDF+AS_PDF3',0,0,'R');
    ln(20);
    
   x := load_ttf_font( getFileFromDatabase('refsan.ttf'), 'CID', p_compress => false );
   set_font( x, 12 );
 
    x:= load_ttf_font( getFileFromDatabase('webdings.ttf'), 'CID', p_compress => false );
   set_font( x, 12 ); 

   x:= load_ttf_font( getFileFromDatabase('freeserif.ttf'), 'CID', p_compress => false );
   set_font( x, 12 );    
   
      x:= load_ttf_font( getFileFromDatabase('freeserifbold.ttf'), 'CID', p_compress => false );
   set_font( x, 12 );  
   
          th(10,'Stnd.');
          th(30,'Family');
          th(8,'Style');
          th(20,'Subtype');
          th(40, 'Name');
          th(35,'fontname');
          th(30, 'encoding');
          th(55, 'charset');
          th(10,'compress'); 
          th(20, 'u_norm');
          th(8, 'cid');
    t_ind := g_used_fonts.first;
    while t_ind is not null
    loop
        if (mod(t_ind,2)!=0) THEN
            SetFillColor(207,202,255);
            tdFill(true);
            tdborder(false);
           
        else
             tdFill(false);
             tdborder(true);
        end if;   
        
       if g_fonts(t_ind).standard then
             tdborder(false);
             td(null, 'Yes');
       else  tdborder(true);
            td(null, 'NO');
       end if;
        td(null, g_fonts(t_ind).Family);
          td(null, g_fonts(t_ind).Style);
          td(null, g_fonts(t_ind).Subtype);
          td('border:1;', g_fonts(t_ind).Name);
          td(null, g_fonts(t_ind).fontname); 
          td(null, g_fonts(t_ind).encoding);
          td(null, g_fonts(t_ind).charset);
          if g_fonts(t_ind).compress_font then
                 td(null, 'Yes');
           else  td(null, 'NO');
           end if;
          td(null, g_fonts(t_ind).unit_norm);
          if g_fonts(t_ind).cid then
                 td(null, 'Yes');
           else  td(null, 'NO');
           end if;
          
          t_ind := g_fonts.next( t_ind );
    end loop;
    tClear;
    AddPage;
     set_font( x-3, 20 );    
    ln(10);
    write(5, 'Font : MSReferenceSansSerif');
    ln(15);
    set_font( x-3, 14 );
    write(5, 'Albanian: Kush mund të lexoni këtë diçka si kjo'); ln(8);
    write(5, 'Croatic: Tko može čitati to nešto poput ovoga');ln(8);
    write(5, 'Russian: Кто может прочитать это что-то вроде этого');ln(8);
    write(5, 'Greek: Ποιος μπορεί να διαβάσει αυτό το κάτι σαν αυτό');ln(10);
    
    ln(10);
    set_font( x-1, 20 );
    write(5, 'Font : FreeSerif');
    ln(15);
    set_font( x-1, 14 );
    write(5, 'Albanian: Kush mund të lexoni këtë diçka si kjo'); ln(8);
    write(5, 'Croatic: Tko može čitati to nešto poput ovoga');ln(8);
    write(5, 'Russian: Кто может прочитать это что-то вроде этого');ln(8);
    write(5, 'Greek: Ποιος μπορεί να διαβάσει αυτό το κάτι σαν αυτό');ln(10);    
    
        ln(10);
    set_font( x, 20 );
    write(5, 'Font : FreeSerifBold');
    ln(15);
    set_font( x, 14 );
    write(5, 'Albanian: Kush mund të lexoni këtë diçka si kjo'); ln(8);
    write(5, 'Croatic: Tko može čitati to nešto poput ovoga');ln(8);
    write(5, 'Russian: Кто может прочитать это что-то вроде этого');ln(8);
    write(5, 'Greek: Ποιος μπορεί να διαβάσει αυτό το κάτι σαν αυτό');ln(10);   

--
   
  output('Fonts.pdf','I');
exception  when others then pdf_error;
end;

procedure barcode_demo is 
  str varchar2(300);
  w_num number DEFAULT 0;
  x pls_integer;
 tHdr tv4000a; -- This is a table for the custom header proc hooked
 t_ind pls_integer;
 begin
    -- reset all variables, arrays....
    resetwpdf;
    
    -- Setting page size/orienation/automatic page change
    pdf('P','mm','letter');
   
    openpdf;
    SetMargins(10,5,5);

    AddPage();
    Set_Font('Times','B', 20);
    Cell(0,15,'Barcode',5,10,'C');
    ln(5);
     Set_Font('Times','B', 16);
    cell(0,0,'GEN_PDF = WPDF+AS_PDF3',0,0,'R');
    ln(20);
--
    for z in (SELECT ROWNUM N, EMPNO, ENAME FROM EMP WHERE ROWNUM <= 8)
    loop
    
        code39((41), 20*z.N+30, z.EMPNO||' '||z.ENAME
                            , 'below', 0.5, 10);
        code39((121), 20*z.N+30, z.EMPNO||' '||z.ENAME
                            , 'smurf', 0.5, 10);
    
    end loop;
   
  output('Barcode.pdf','I');
exception  when others then   pdf_error;
end;

procedure rotate_demo is 
  str varchar2(300);
  w_num number DEFAULT 0;
  x pls_integer;
 tHdr tv4000a; -- This is a table for the custom header proc hooked
 t_ind pls_integer;
 begin
    -- reset all variables, arrays....
    resetwpdf;
    
    -- Setting page size/orienation/automatic page change
    pdf('P','mm','letter');
   
    openpdf;
    SetMargins(10,5,5);
    AddPage();
    Set_Font('Times','B', 20);
    Cell(0,15,'Rotate',5,10,'C');
    ln(5);
     Set_Font('Times','B', 16);
    cell(0,0,'GEN_PDF = WPDF+AS_PDF3',0,0,'R');
    ln(20);
    
    

      for z in 1..13 loop
        Rect(20,40+2*z, 100,1*z/10,'F');
      end loop;   
      
      for z in 1..13 loop
        Rect(150+2*z,40, 1*z/10,100,'FD');
      end loop; 
      
      for z in 1..6 loop
            text(100,150,'   Rotate this text '||60*z||' deegres', 60*z);
      end loop;
      Set_Font('Times','B', 17);
      Rect(18,38, 104,35,null);
      RoundedRect(25, 75, 150, 150, 75, '1234', null);
      RoundedRect(10, 15, 10, 10, 5, '1234', 'F');
      RoundedRect(10, 25, 20, 10, 2, '1234', null);
      RoundedRect(30, 25, 25, 10, 3, '14', null);
      RoundedRect(55, 25, 25, 10, 3, '23', null);
      
  output('Rotate.pdf','I');
exception  when others then   pdf_error;
end;

procedure images_demo is 
  t_logo varchar2(32767) := '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';
    str varchar2(300);
  w_num number DEFAULT 1;
  x pls_integer;
 tHdr tv4000a; -- This is a table for the custom header proc hooked
 t_ind pls_integer;
 begin
    -- reset all variables, arrays....
    resetwpdf;
    
    -- Setting page size/orienation/automatic page change
    pdf('P','mm','letter');
   
    openpdf;
    SetMargins(10,5,5);
    AddPage();
    Set_Font('Times','B', 22);
    Cell(0,15,'Images',5,10,'C');
    ln(5);
     Set_Font('Times','B', 16);
    cell(0,0,'GEN_PDF = WPDF+AS_PDF3',0,0,'R');
    ln(20);
    image(t_logo, 25, 50, 100, 100); 
    
    for z IN (SELECT ROWNUM n, Filename
          FROM WWV_FLOW_FILES 
         WHERE Filename IN ('pdf.jpg','linux_256.jpg'--,'Mushroom2.PNG'--,'cat.jpg'
          ))
     loop
        if mod(z.n, 2)=0 then
            Image(getFileFromDatabase(z.Filename), 310,  100+100*w_num, 64, 64, null);
            w_num:=w_num+1;
        else
            Image(getFileFromDatabase(z.Filename), 20, 100+100*w_num, 64, 64, null);
        end if;
        
     end loop;
     
     text(50, 150,'Works in blob''s or clob BASE64 (png, jpg)',60);
          
  output('Images.pdf','I');
exception  when others then   pdf_error;
end;

procedure chart_demo is 
  begin
    -- reset all variables, arrays....
    resetwpdf;
    
    -- Setting page size/orienation/automatic page change
    pdf('P','mm','letter');
   
    openpdf;
    SetMargins(10,5,5);
    AddPage();
    Set_Font('Times','B', 22);
    Cell(0,15,'Other Google API Chart / QR code',5,10,'C');
    ln(5);
     Set_Font('Times','B', 16);
    cell(0,0,'GEN_PDF = WPDF+AS_PDF3',0,0,'R');
    ln(5);
    Set_Font('Times','N', 10);
    write(5, 'For this sample Grant UTL_HTTP and add ACL for (Page 2 sample):');ln(5);
    write(5, ' - chart.googleapis.com');ln(5);
    write(5, ' - zxing.org');ln(5);
    write(5, 'Basically we call API to generate image and then add image to PDF');ln(15);
    
    
   
    image( httpuritype (get_emps_chart()).getblob ()
          ,100,140,0,0);
    write(5, get_emps_chart());     
    
    image( httpuritype ('zxing.org/w/chart?'||'cht=qr&'||'chs=100x100&'||'chld=L&'||'choe=UTF-8&'||'chl=http%3A%2F%2Fgoogle.com').getblob ()
          ,250,650,0,0);
     ln(150);
     write(5, 'https://zxing.org/w/chart?'||'cht=qr&'||'chs=100x100&'||'chld=L&'||'choe=UTF-8&'||'chl=http%3A%2F%2Fgoogle.com');
     AddPage();
     ln(20);
     write(5, 'BEGIN');ln(5);
     write(5, '   DBMS_NETWORK_ACL_ADMIN.CREATE_ACL (');ln(5);
     write(5, '    acl          => ''chart.xml'',');ln(5);
     write(5, '    description  => ''Permissions to get chart image'',');ln(5);
     write(5, '    principal    => ''APEX_050000'', -- USER');ln(5);
     write(5, '    is_grant     => TRUE,');ln(5);
     write(5, '    privilege    => ''connect'');');ln(5);
     write(5, '   COMMIT;');ln(5);
     write(5, 'END;');ln(5);
     write(5, '/');ln(5);
     write(5, 'BEGIN');
     write(5, '   DBMS_NETWORK_ACL_ADMIN.ASSIGN_ACL (');ln(5);
     write(5, '    acl          => ''chart.xml'','); ln(5);               
     write(5, '    host         => ''chart.googleapis.com'');');ln(5);
     write(5, '   COMMIT;');ln(5);
     write(5, 'END;');ln(5);
     write(5, '/');ln(5);
     write(5, 'BEGIN');ln(5);
     write(5, '   DBMS_NETWORK_ACL_ADMIN.ASSIGN_ACL (');ln(5);
     write(5, '    acl          => ''chart.xml'','); ln(5);              
     write(5, '    host         => ''zxing.org'');');ln(5);
     write(5, '   COMMIT;');ln(5);
     write(5, 'END;');ln(5);
     write(5, '/');ln(5);
    
    output('Chart.pdf','I');
   
exception  when others then   pdf_error;
end;


procedure pdf_error is
begin

  rollback;

htp.p('
<html>
<head>
    <title>PDF Error</title>
</head>
<body>
<br>

A PDF error has occurred.
 <br>
 <br>

'|| OWA_UTIL.GET_PROCEDURE ||'

<br>


Code: '|| SQLCODE  ||' <br>
Code: '|| SQLERRM  ||' <br>

<br>
'|| replace(dbms_utility.format_error_backtrace,'ORA-', '<br>ORA-') ||'
<br>
<br>
');


  if true or gb_mode_debug then
      print('<pre>');
      htpc(clobfromblob(pdfBoc));
      print('</pre>');
    end if;

htp.p('

</body>
</html>
');


end;

/*
  procedure save_pdf
    ( p_dir varchar2 := 'MY_DIR'
    , p_filename varchar2 := 'myPDF.pdf'
    , p_freeblob boolean := true
    )
  is
    t_fh utl_file.file_type;
    t_len pls_integer := 32767;
  begin
    
    if state < 3 then
        ClosePDF();
    end if;
    t_fh := utl_file.fopen( p_dir, p_filename, 'wb' );
    for i in 0 .. trunc( ( dbms_lob.getlength( pdfBoc ) - 1 ) / t_len )
    loop
      utl_file.put_raw( t_fh
                      , dbms_lob.substr( pdfBoc
                                       , t_len
                                       , i * t_len + 1
                                       )
                      );
    end loop;
    utl_file.fclose( t_fh );
    if p_freeblob
    then
      dbms_lob.freetemporary( pdfBoc );
    end if;
  end;
*/

END;
/