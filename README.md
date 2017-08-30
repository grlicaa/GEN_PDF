# GEN_PDF

## Demo
A demo application is available on apex.oracle.com<br/>
https://apex.oracle.com/pls/apex/f?p=89939


## Changelog
V 1.0.
<ul>
<li>Release date : 14/10/2015</li>
</ul>


## About

GEN_PDF is PL/SQL based generator for PDF.

GEN_PDF is mix of WPDF(1.4.8b) and AS_PDF3.

Why ?
- WPDF is great for setting position of custom tables, colors, borders ...
- AS_PDF3 is great for importing fonts into PDF

GEN_PDF is set to work on apex. Fonts and images are stored in WWV_FLOW_FILES or CUSTOM table, so there is no need to use UTL.FILE package (like in AS_PDF3)
and there is also no need for "ordsys.ordImage" (like in PL_FPDF which don't work on XE db).

You can find more info inside "GEN_PDF.pks"



## How to use it

Inside package are few samples which demonstrate almost all functionality of GEN_PDF tool.


### List of samples inside package :
<ul>
<li>procedure emp_demo</li>
<li>procedure fonts_demo</li>
<li>procedure barcode_demo</li>
<li>procedure rotate_demo</li>
<li>procedure images_demo</li>
<li>procedure testHeader</li>
<li>procedure chart_demo</li>
</ul>

### Run sample:
 
 Add apex Non-Modal dialog page and before header in process add PL/SQL code
 ```sql
BEGIN
   gen_pdf.emp_demo();
   apex_application.stop_apex_engine;
END;
```

### Needed Rights
For last sample "chart_demo" to work you need  :
1. Grant on UTL_HTTP
```sql
  GRANT EXECUTE ON UTL_HTTP TO <USER>;
```
2. ACL permissions to get Google chart and QR code

```sql
     BEGIN
        DBMS_NETWORK_ACL_ADMIN.CREATE_ACL (
            acl          => 'chart.xml',
            description  => 'Permissions to get chart image',
            principal    => 'APEX_050000', -- APEX_SCHEMA depands on APEX version
            is_grant     => TRUE,
            privilege    => 'connect');
        COMMIT;
     END;
     /
     BEGIN
        DBMS_NETWORK_ACL_ADMIN.ASSIGN_ACL (
            acl          => 'chart.xml',
            host         => 'chart.googleapis.com');
        COMMIT;
        END;
     /
     BEGIN
        DBMS_NETWORK_ACL_ADMIN.ASSIGN_ACL (
            acl          => 'chart.xml',
            host         => 'zxing.org');
        COMMIT;
     END;
     /
```
     
In case you need to save file on server you can enable procedure "save_pdf".

If you need reed file from server "font or pdf" than enable function "file2blob".

For this to work you need grant on UTL_FILE.<br/>
```sql
  GRANT EXECUTE ON UTL_FILE TO <USER>;
```
   


## About me
Andrej Grlica<br/>
I’m a oracle apex developer since 2008.<br/>
Work email : [andrej.grlica@abakus.si](mailto:andrej.grlica@abakus.si)<br/>
Private email : [andrej.grlica@gmail.com](mailto:andrej.grlica@gmail.com)<br/>
Twitter : [@AndrejGrlica](https://twitter.com/AndrejGrlica)<br/>
Linked-in : [Link](https://www.linkedin.com/in/andrej-grlica-303998a4/)<br/>
Slack (#orclapex) PM:[@grlicaa](https://orclapex.slack.com/messages/@grlicaa/)

