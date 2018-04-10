 module cjson_wrapper_mod
 
  use cjson_args_mod,      only: json_call
  use cjson_error_mod,     only: cjson_error_mesg, FATAL, WARNING, NOTE
  use cjson_utils_mod,     only: strcase
  use cjson_jsonout_mod,   only: cjson_write_json_object,OBJ_PTR,OUTPUT_JSON
  use iso_c_binding
  implicit none
 
  interface
   integer function namelist_exist (my_json_string, nml_name) bind (C,name="namelist_exist")
     use iso_c_binding
     character(kind=c_char) :: nml_name       !< The name of the namelist that the variable is in 
     character(kind=c_char) :: my_json_string !< The string containing the JSON
   end function
  end interface
 contains
 
 SUBROUTINE JSON_ARGS (                                                  &   
 NMLNAME, JSON, CJSON,                                                   &   
 var1, varname1, objname1, st_size1,  &
 var2, varname2, objname2, st_size2,  &
 var3, varname3, objname3, st_size3,  &
 var4, varname4, objname4, st_size4,  &
 var5, varname5, objname5, st_size5,  &
 var6, varname6, objname6, st_size6,  &
 var7, varname7, objname7, st_size7,  &
 var8, varname8, objname8, st_size8,  &
 var9, varname9, objname9, st_size9,  &
 var10, varname10, objname10, st_size10,  &
 var11, varname11, objname11, st_size11,  &
 var12, varname12, objname12, st_size12,  &
 var13, varname13, objname13, st_size13,  &
 var14, varname14, objname14, st_size14,  &
 var15, varname15, objname15, st_size15,  &
 var16, varname16, objname16, st_size16,  &
 var17, varname17, objname17, st_size17,  &
 var18, varname18, objname18, st_size18,  &
 var19, varname19, objname19, st_size19,  &
 var20, varname20, objname20, st_size20,  &
 var21, varname21, objname21, st_size21,  &
 var22, varname22, objname22, st_size22,  &
 var23, varname23, objname23, st_size23,  &
 var24, varname24, objname24, st_size24,  &
 var25, varname25, objname25, st_size25,  &
 var26, varname26, objname26, st_size26,  &
 var27, varname27, objname27, st_size27,  &
 var28, varname28, objname28, st_size28,  &
 var29, varname29, objname29, st_size29,  &
 var30, varname30, objname30, st_size30,  &
 var31, varname31, objname31, st_size31,  &
 var32, varname32, objname32, st_size32,  &
 var33, varname33, objname33, st_size33,  &
 var34, varname34, objname34, st_size34,  &
 var35, varname35, objname35, st_size35,  &
 var36, varname36, objname36, st_size36,  &
 var37, varname37, objname37, st_size37,  &
 var38, varname38, objname38, st_size38,  &
 var39, varname39, objname39, st_size39,  &
 var40, varname40, objname40, st_size40,  &
 var41, varname41, objname41, st_size41,  &
 var42, varname42, objname42, st_size42,  &
 var43, varname43, objname43, st_size43,  &
 var44, varname44, objname44, st_size44,  &
 var45, varname45, objname45, st_size45,  &
 var46, varname46, objname46, st_size46,  &
 var47, varname47, objname47, st_size47,  &
 var48, varname48, objname48, st_size48,  &
 var49, varname49, objname49, st_size49,  &
 var50, varname50, objname50, st_size50,  &
 var51, varname51, objname51, st_size51,  &
 var52, varname52, objname52, st_size52,  &
 var53, varname53, objname53, st_size53,  &
 var54, varname54, objname54, st_size54,  &
 var55, varname55, objname55, st_size55,  &
 var56, varname56, objname56, st_size56,  &
 var57, varname57, objname57, st_size57,  &
 var58, varname58, objname58, st_size58,  &
 var59, varname59, objname59, st_size59,  &
 var60, varname60, objname60, st_size60,  &
 var61, varname61, objname61, st_size61,  &
 var62, varname62, objname62, st_size62,  &
 var63, varname63, objname63, st_size63,  &
 var64, varname64, objname64, st_size64,  &
 var65, varname65, objname65, st_size65,  &
 var66, varname66, objname66, st_size66,  &
 var67, varname67, objname67, st_size67,  &
 var68, varname68, objname68, st_size68,  &
 var69, varname69, objname69, st_size69,  &
 var70, varname70, objname70, st_size70,  &
 var71, varname71, objname71, st_size71,  &
 var72, varname72, objname72, st_size72,  &
 var73, varname73, objname73, st_size73,  &
 var74, varname74, objname74, st_size74,  &
 var75, varname75, objname75, st_size75,  &
 var76, varname76, objname76, st_size76,  &
 var77, varname77, objname77, st_size77,  &
 var78, varname78, objname78, st_size78,  &
 var79, varname79, objname79, st_size79,  &
 var80, varname80, objname80, st_size80,  &
 var81, varname81, objname81, st_size81,  &
 var82, varname82, objname82, st_size82,  &
 var83, varname83, objname83, st_size83,  &
 var84, varname84, objname84, st_size84,  &
 var85, varname85, objname85, st_size85,  &
 var86, varname86, objname86, st_size86,  &
 var87, varname87, objname87, st_size87,  &
 var88, varname88, objname88, st_size88,  &
 var89, varname89, objname89, st_size89,  &
 var90, varname90, objname90, st_size90,  &
 var91, varname91, objname91, st_size91,  &
 var92, varname92, objname92, st_size92,  &
 var93, varname93, objname93, st_size93,  &
 var94, varname94, objname94, st_size94,  &
 var95, varname95, objname95, st_size95,  &
 array1, arrayname1, arrayobj1, arrayst_size1, lowb1,   &
 array2, arrayname2, arrayobj2, arrayst_size2, lowb2,   &
 array3, arrayname3, arrayobj3, arrayst_size3, lowb3,   &
 array4, arrayname4, arrayobj4, arrayst_size4, lowb4,   &
 array5, arrayname5, arrayobj5, arrayst_size5, lowb5,   &
 array6, arrayname6, arrayobj6, arrayst_size6, lowb6,   &
 array7, arrayname7, arrayobj7, arrayst_size7, lowb7,   &
 array8, arrayname8, arrayobj8, arrayst_size8, lowb8,   &
 array9, arrayname9, arrayobj9, arrayst_size9, lowb9,   &
 array10, arrayname10, arrayobj10, arrayst_size10, lowb10,   &
 array11, arrayname11, arrayobj11, arrayst_size11, lowb11,   &
 array12, arrayname12, arrayobj12, arrayst_size12, lowb12,   &
 array13, arrayname13, arrayobj13, arrayst_size13, lowb13,   &
 array14, arrayname14, arrayobj14, arrayst_size14, lowb14,   &
 array15, arrayname15, arrayobj15, arrayst_size15, lowb15,   &
 array16, arrayname16, arrayobj16, arrayst_size16, lowb16,   &
 array17, arrayname17, arrayobj17, arrayst_size17, lowb17,   &
 array18, arrayname18, arrayobj18, arrayst_size18, lowb18,   &
 array19, arrayname19, arrayobj19, arrayst_size19, lowb19,   &
 array20, arrayname20, arrayobj20, arrayst_size20, lowb20,   &
 array21, arrayname21, arrayobj21, arrayst_size21, lowb21,   &
 array22, arrayname22, arrayobj22, arrayst_size22, lowb22,   &
 array23, arrayname23, arrayobj23, arrayst_size23, lowb23,   &
 array24, arrayname24, arrayobj24, arrayst_size24, lowb24,   &
 array25, arrayname25, arrayobj25, arrayst_size25, lowb25,   &
 array26, arrayname26, arrayobj26, arrayst_size26, lowb26,   &
 array27, arrayname27, arrayobj27, arrayst_size27, lowb27,   &
 array28, arrayname28, arrayobj28, arrayst_size28, lowb28,   &
 array29, arrayname29, arrayobj29, arrayst_size29, lowb29,   &
 array30, arrayname30, arrayobj30, arrayst_size30, lowb30,   &
 array31, arrayname31, arrayobj31, arrayst_size31, lowb31,   &
 array32, arrayname32, arrayobj32, arrayst_size32, lowb32,   &
 array33, arrayname33, arrayobj33, arrayst_size33, lowb33,   &
 array34, arrayname34, arrayobj34, arrayst_size34, lowb34,   &
 array35, arrayname35, arrayobj35, arrayst_size35, lowb35,   &
 array36, arrayname36, arrayobj36, arrayst_size36, lowb36,   &
 array37, arrayname37, arrayobj37, arrayst_size37, lowb37,   &
 array38, arrayname38, arrayobj38, arrayst_size38, lowb38,   &
 array39, arrayname39, arrayobj39, arrayst_size39, lowb39,   &
 array40, arrayname40, arrayobj40, arrayst_size40, lowb40,   &
 array41, arrayname41, arrayobj41, arrayst_size41, lowb41,   &
 array42, arrayname42, arrayobj42, arrayst_size42, lowb42,   &
 array43, arrayname43, arrayobj43, arrayst_size43, lowb43,   &
 array44, arrayname44, arrayobj44, arrayst_size44, lowb44,   &
 array45, arrayname45, arrayobj45, arrayst_size45, lowb45,   &
 array46, arrayname46, arrayobj46, arrayst_size46, lowb46,   &
 array47, arrayname47, arrayobj47, arrayst_size47, lowb47,   &
 array48, arrayname48, arrayobj48, arrayst_size48, lowb48,   &
 array49, arrayname49, arrayobj49, arrayst_size49, lowb49,   &
 array50, arrayname50, arrayobj50, arrayst_size50, lowb50,   &
 array51, arrayname51, arrayobj51, arrayst_size51, lowb51,   &
 array52, arrayname52, arrayobj52, arrayst_size52, lowb52,   &
 array53, arrayname53, arrayobj53, arrayst_size53, lowb53,   &
 array54, arrayname54, arrayobj54, arrayst_size54, lowb54,   &
 array55, arrayname55, arrayobj55, arrayst_size55, lowb55,   &
 array56, arrayname56, arrayobj56, arrayst_size56, lowb56,   &
 array57, arrayname57, arrayobj57, arrayst_size57, lowb57,   &
 array58, arrayname58, arrayobj58, arrayst_size58, lowb58,   &
 array59, arrayname59, arrayobj59, arrayst_size59, lowb59,   &
 array60, arrayname60, arrayobj60, arrayst_size60, lowb60,   &
 array61, arrayname61, arrayobj61, arrayst_size61, lowb61,   &
 array62, arrayname62, arrayobj62, arrayst_size62, lowb62,   &
 array63, arrayname63, arrayobj63, arrayst_size63, lowb63,   &
 array64, arrayname64, arrayobj64, arrayst_size64, lowb64,   &
 array65, arrayname65, arrayobj65, arrayst_size65, lowb65,   &
 array66, arrayname66, arrayobj66, arrayst_size66, lowb66,   &
 array67, arrayname67, arrayobj67, arrayst_size67, lowb67,   &
 array68, arrayname68, arrayobj68, arrayst_size68, lowb68,   &
 array69, arrayname69, arrayobj69, arrayst_size69, lowb69,   &
 array70, arrayname70, arrayobj70, arrayst_size70, lowb70,   &
 array71, arrayname71, arrayobj71, arrayst_size71, lowb71,   &
 array72, arrayname72, arrayobj72, arrayst_size72, lowb72,   &
 array73, arrayname73, arrayobj73, arrayst_size73, lowb73,   &
 array74, arrayname74, arrayobj74, arrayst_size74, lowb74,   &
 array75, arrayname75, arrayobj75, arrayst_size75, lowb75,   &
 array76, arrayname76, arrayobj76, arrayst_size76, lowb76,   &
 array77, arrayname77, arrayobj77, arrayst_size77, lowb77,   &
 array78, arrayname78, arrayobj78, arrayst_size78, lowb78,   &
 array79, arrayname79, arrayobj79, arrayst_size79, lowb79,   &
 array80, arrayname80, arrayobj80, arrayst_size80, lowb80,   &
 array81, arrayname81, arrayobj81, arrayst_size81, lowb81,   &
 array82, arrayname82, arrayobj82, arrayst_size82, lowb82,   &
 array83, arrayname83, arrayobj83, arrayst_size83, lowb83,   &
 array84, arrayname84, arrayobj84, arrayst_size84, lowb84,   &
 array85, arrayname85, arrayobj85, arrayst_size85, lowb85,   &
 array86, arrayname86, arrayobj86, arrayst_size86, lowb86,   &
 array87, arrayname87, arrayobj87, arrayst_size87, lowb87,   &
 array88, arrayname88, arrayobj88, arrayst_size88, lowb88,   &
 array89, arrayname89, arrayobj89, arrayst_size89, lowb89,   &
 array90, arrayname90, arrayobj90, arrayst_size90, lowb90,   &
 array91, arrayname91, arrayobj91, arrayst_size91, lowb91,   &
 array92, arrayname92, arrayobj92, arrayst_size92, lowb92,   &
 array93, arrayname93, arrayobj93, arrayst_size93, lowb93,   &
 array94, arrayname94, arrayobj94, arrayst_size94, lowb94,   &
 array95, arrayname95, arrayobj95, arrayst_size95, lowb95,   &
 a2d1, a2dname1, a2dobj1, a2dst_size1, ilowb1,jlowb1,  &
 a2d2, a2dname2, a2dobj2, a2dst_size2, ilowb2,jlowb2,  &
 a2d3, a2dname3, a2dobj3, a2dst_size3, ilowb3,jlowb3,  &
 a2d4, a2dname4, a2dobj4, a2dst_size4, ilowb4,jlowb4,  &
 a2d5, a2dname5, a2dobj5, a2dst_size5, ilowb5,jlowb5,  &
 a2d6, a2dname6, a2dobj6, a2dst_size6, ilowb6,jlowb6,  &
 a2d7, a2dname7, a2dobj7, a2dst_size7, ilowb7,jlowb7,  &
 a2d8, a2dname8, a2dobj8, a2dst_size8, ilowb8,jlowb8,  &
 a2d9, a2dname9, a2dobj9, a2dst_size9, ilowb9,jlowb9,  &
 a2d10, a2dname10, a2dobj10, a2dst_size10, ilowb10,jlowb10,  &
 a2d11, a2dname11, a2dobj11, a2dst_size11, ilowb11,jlowb11,  &
 a2d12, a2dname12, a2dobj12, a2dst_size12, ilowb12,jlowb12,  &
 a2d13, a2dname13, a2dobj13, a2dst_size13, ilowb13,jlowb13,  &
 a2d14, a2dname14, a2dobj14, a2dst_size14, ilowb14,jlowb14,  &
 a2d15, a2dname15, a2dobj15, a2dst_size15, ilowb15,jlowb15,  &
 a2d16, a2dname16, a2dobj16, a2dst_size16, ilowb16,jlowb16,  &
 a2d17, a2dname17, a2dobj17, a2dst_size17, ilowb17,jlowb17,  &
 a2d18, a2dname18, a2dobj18, a2dst_size18, ilowb18,jlowb18,  &
 a2d19, a2dname19, a2dobj19, a2dst_size19, ilowb19,jlowb19,  &
 a2d20, a2dname20, a2dobj20, a2dst_size20, ilowb20,jlowb20  )
  character (*)                          :: nmlname 
  character (*), intent (in)             :: json    
  type (C_PTR), INTENT (IN)              :: cjson    !< The json string
 class (*),optional,intent(inout)           :: &
                    var1, var2, var3, var4, var5, &
                    array1(:), array2(:), array3(:), array4(:), array5(:), &
                    a2d1(:,:), a2d2(:,:), a2d3(:,:), a2d4(:,:), a2d5(:,:), &
                    var6, var7, var8, var9, var10, &
                    array6(:), array7(:), array8(:), array9(:), array10(:), &
                    a2d6(:,:), a2d7(:,:), a2d8(:,:), a2d9(:,:), a2d10(:,:), &
                   var11,var12,var13,var14,var15, &
                   array11(:),array12(:),array13(:),array14(:),array15(:), &
                   a2d11(:,:),a2d12(:,:),a2d13(:,:),a2d14(:,:),a2d15(:,:), &
                   var16,var17,var18,var19,var20, &
                   array16(:),array17(:),array18(:),array19(:),array20(:), &
                   a2d16(:,:),a2d17(:,:),a2d18(:,:),a2d19(:,:),a2d20(:,:), &
                   var21,var22,var23,var24,var25, &
                   array21(:),array22(:),array23(:),array24(:),array25(:), &
                   var26,var27,var28,var29,var30, &
                   array26(:),array27(:),array28(:),array29(:),array30(:), &
                   var31,var32,var33,var34,var35, &
                   array31(:),array32(:),array33(:),array34(:),array35(:), &
                   var36,var37,var38,var39,var40, &
                   array36(:),array37(:),array38(:),array39(:),array40(:), &
                   var41,var42,var43,var44,var45, &
                   array41(:),array42(:),array43(:),array44(:),array45(:), &
                   var46,var47,var48,var49,var50, &
                   array46(:),array47(:),array48(:),array49(:),array50(:), &
                   var51,var52,var53,var54,var55, &
                   array51(:),array52(:),array53(:),array54(:),array55(:), &
                   var56,var57,var58,var59,var60, &
                   array56(:),array57(:),array58(:),array59(:),array60(:), &
                   var61,var62,var63,var64,var65, &
                   array61(:),array62(:),array63(:),array64(:),array65(:), &
                   var66,var67,var68,var69,var70, &
                   array66(:),array67(:),array68(:),array69(:),array70(:), &
                   var71,var72,var73,var74,var75, &
                   array71(:),array72(:),array73(:),array74(:),array75(:), &
                   var76,var77,var78,var79,var80, &
                   array76(:),array77(:),array78(:),array79(:),array80(:), &
                   var81,var82,var83,var84,var85, &
                   array81(:),array82(:),array83(:),array84(:),array85(:), &
                   var86,var87,var88,var89,var90, &
                   array86(:),array87(:),array88(:),array89(:),array90(:), &
                   var91,var92,var93,var94,var95, &
                   array91(:),array92(:),array93(:),array94(:),array95(:)
 character (*),optional,intent(in)           :: &
                    varname1, varname2, varname3, varname4, varname5, &
                    arrayname1, arrayname2, arrayname3, arrayname4, arrayname5, &
                    a2dname1, a2dname2, a2dname3, a2dname4, a2dname5, &
                    varname6, varname7, varname8, varname9, varname10, &
                    arrayname6, arrayname7, arrayname8, arrayname9, arrayname10, &
                    a2dname6, a2dname7, a2dname8, a2dname9, a2dname10, &
                   varname11,varname12,varname13,varname14,varname15, &
                   arrayname11,arrayname12,arrayname13,arrayname14,arrayname15, &
                   a2dname11,a2dname12,a2dname13,a2dname14,a2dname15, &
                   varname16,varname17,varname18,varname19,varname20, &
                   arrayname16,arrayname17,arrayname18,arrayname19,arrayname20, &
                   a2dname16,a2dname17,a2dname18,a2dname19,a2dname20, &
                   varname21,varname22,varname23,varname24,varname25, &
                   arrayname21,arrayname22,arrayname23,arrayname24,arrayname25, &
                   varname26,varname27,varname28,varname29,varname30, &
                   arrayname26,arrayname27,arrayname28,arrayname29,arrayname30, &
                   varname31,varname32,varname33,varname34,varname35, &
                   arrayname31,arrayname32,arrayname33,arrayname34,arrayname35, &
                   varname36,varname37,varname38,varname39,varname40, &
                   arrayname36,arrayname37,arrayname38,arrayname39,arrayname40, &
                   varname41,varname42,varname43,varname44,varname45, &
                   arrayname41,arrayname42,arrayname43,arrayname44,arrayname45, &
                   varname46,varname47,varname48,varname49,varname50, &
                   arrayname46,arrayname47,arrayname48,arrayname49,arrayname50, &
                   varname51,varname52,varname53,varname54,varname55, &
                   arrayname51,arrayname52,arrayname53,arrayname54,arrayname55, &
                   varname56,varname57,varname58,varname59,varname60, &
                   arrayname56,arrayname57,arrayname58,arrayname59,arrayname60, &
                   varname61,varname62,varname63,varname64,varname65, &
                   arrayname61,arrayname62,arrayname63,arrayname64,arrayname65, &
                   varname66,varname67,varname68,varname69,varname70, &
                   arrayname66,arrayname67,arrayname68,arrayname69,arrayname70, &
                   varname71,varname72,varname73,varname74,varname75, &
                   arrayname71,arrayname72,arrayname73,arrayname74,arrayname75, &
                   varname76,varname77,varname78,varname79,varname80, &
                   arrayname76,arrayname77,arrayname78,arrayname79,arrayname80, &
                   varname81,varname82,varname83,varname84,varname85, &
                   arrayname81,arrayname82,arrayname83,arrayname84,arrayname85, &
                   varname86,varname87,varname88,varname89,varname90, &
                   arrayname86,arrayname87,arrayname88,arrayname89,arrayname90, &
                   varname91,varname92,varname93,varname94,varname95, &
                   arrayname91,arrayname92,arrayname93,arrayname94,arrayname95
 character (*),optional,intent(in)           :: &
                    objname1, objname2, objname3, objname4, objname5, &
                    arrayobj1, arrayobj2, arrayobj3, arrayobj4, arrayobj5, &
                    a2dobj1, a2dobj2, a2dobj3, a2dobj4, a2dobj5, &
                    objname6, objname7, objname8, objname9, objname10, &
                    arrayobj6, arrayobj7, arrayobj8, arrayobj9, arrayobj10, &
                    a2dobj6, a2dobj7, a2dobj8, a2dobj9, a2dobj10, &
                   objname11,objname12,objname13,objname14,objname15, &
                   arrayobj11,arrayobj12,arrayobj13,arrayobj14,arrayobj15, &
                   a2dobj11,a2dobj12,a2dobj13,a2dobj14,a2dobj15, &
                   objname16,objname17,objname18,objname19,objname20, &
                   arrayobj16,arrayobj17,arrayobj18,arrayobj19,arrayobj20, &
                   a2dobj16,a2dobj17,a2dobj18,a2dobj19,a2dobj20, &
                   objname21,objname22,objname23,objname24,objname25, &
                   arrayobj21,arrayobj22,arrayobj23,arrayobj24,arrayobj25, &
                   objname26,objname27,objname28,objname29,objname30, &
                   arrayobj26,arrayobj27,arrayobj28,arrayobj29,arrayobj30, &
                   objname31,objname32,objname33,objname34,objname35, &
                   arrayobj31,arrayobj32,arrayobj33,arrayobj34,arrayobj35, &
                   objname36,objname37,objname38,objname39,objname40, &
                   arrayobj36,arrayobj37,arrayobj38,arrayobj39,arrayobj40, &
                   objname41,objname42,objname43,objname44,objname45, &
                   arrayobj41,arrayobj42,arrayobj43,arrayobj44,arrayobj45, &
                   objname46,objname47,objname48,objname49,objname50, &
                   arrayobj46,arrayobj47,arrayobj48,arrayobj49,arrayobj50, &
                   objname51,objname52,objname53,objname54,objname55, &
                   arrayobj51,arrayobj52,arrayobj53,arrayobj54,arrayobj55, &
                   objname56,objname57,objname58,objname59,objname60, &
                   arrayobj56,arrayobj57,arrayobj58,arrayobj59,arrayobj60, &
                   objname61,objname62,objname63,objname64,objname65, &
                   arrayobj61,arrayobj62,arrayobj63,arrayobj64,arrayobj65, &
                   objname66,objname67,objname68,objname69,objname70, &
                   arrayobj66,arrayobj67,arrayobj68,arrayobj69,arrayobj70, &
                   objname71,objname72,objname73,objname74,objname75, &
                   arrayobj71,arrayobj72,arrayobj73,arrayobj74,arrayobj75, &
                   objname76,objname77,objname78,objname79,objname80, &
                   arrayobj76,arrayobj77,arrayobj78,arrayobj79,arrayobj80, &
                   objname81,objname82,objname83,objname84,objname85, &
                   arrayobj81,arrayobj82,arrayobj83,arrayobj84,arrayobj85, &
                   objname86,objname87,objname88,objname89,objname90, &
                   arrayobj86,arrayobj87,arrayobj88,arrayobj89,arrayobj90, &
                   objname91,objname92,objname93,objname94,objname95, &
                   arrayobj91,arrayobj92,arrayobj93,arrayobj94,arrayobj95
 integer,optional,intent(in)           ::   &
                    st_size1, st_size2, st_size3, st_size4, st_size5, &
                    arrayst_size1, arrayst_size2, arrayst_size3, arrayst_size4, arrayst_size5, &
                    a2dst_size1, a2dst_size2, a2dst_size3, a2dst_size4, a2dst_size5, &
                    st_size6, st_size7, st_size8, st_size9, st_size10, &
                    arrayst_size6, arrayst_size7, arrayst_size8, arrayst_size9, arrayst_size10, &
                    a2dst_size6, a2dst_size7, a2dst_size8, a2dst_size9, a2dst_size10, &
                   st_size11,st_size12,st_size13,st_size14,st_size15, &
                   arrayst_size11,arrayst_size12,arrayst_size13,arrayst_size14,arrayst_size15, &
                   a2dst_size11,a2dst_size12,a2dst_size13,a2dst_size14,a2dst_size15, &
                   st_size16,st_size17,st_size18,st_size19,st_size20, &
                   arrayst_size16,arrayst_size17,arrayst_size18,arrayst_size19,arrayst_size20, &
                   a2dst_size16,a2dst_size17,a2dst_size18,a2dst_size19,a2dst_size20, &
                   st_size21,st_size22,st_size23,st_size24,st_size25, &
                   arrayst_size21,arrayst_size22,arrayst_size23,arrayst_size24,arrayst_size25, &
                   st_size26,st_size27,st_size28,st_size29,st_size30, &
                   arrayst_size26,arrayst_size27,arrayst_size28,arrayst_size29,arrayst_size30, &
                   st_size31,st_size32,st_size33,st_size34,st_size35, &
                   arrayst_size31,arrayst_size32,arrayst_size33,arrayst_size34,arrayst_size35, &
                   st_size36,st_size37,st_size38,st_size39,st_size40, &
                   arrayst_size36,arrayst_size37,arrayst_size38,arrayst_size39,arrayst_size40, &
                   st_size41,st_size42,st_size43,st_size44,st_size45, &
                   arrayst_size41,arrayst_size42,arrayst_size43,arrayst_size44,arrayst_size45, &
                   st_size46,st_size47,st_size48,st_size49,st_size50, &
                   arrayst_size46,arrayst_size47,arrayst_size48,arrayst_size49,arrayst_size50, &
                   st_size51,st_size52,st_size53,st_size54,st_size55, &
                   arrayst_size51,arrayst_size52,arrayst_size53,arrayst_size54,arrayst_size55, &
                   st_size56,st_size57,st_size58,st_size59,st_size60, &
                   arrayst_size56,arrayst_size57,arrayst_size58,arrayst_size59,arrayst_size60, &
                   st_size61,st_size62,st_size63,st_size64,st_size65, &
                   arrayst_size61,arrayst_size62,arrayst_size63,arrayst_size64,arrayst_size65, &
                   st_size66,st_size67,st_size68,st_size69,st_size70, &
                   arrayst_size66,arrayst_size67,arrayst_size68,arrayst_size69,arrayst_size70, &
                   st_size71,st_size72,st_size73,st_size74,st_size75, &
                   arrayst_size71,arrayst_size72,arrayst_size73,arrayst_size74,arrayst_size75, &
                   st_size76,st_size77,st_size78,st_size79,st_size80, &
                   arrayst_size76,arrayst_size77,arrayst_size78,arrayst_size79,arrayst_size80, &
                   st_size81,st_size82,st_size83,st_size84,st_size85, &
                   arrayst_size81,arrayst_size82,arrayst_size83,arrayst_size84,arrayst_size85, &
                   st_size86,st_size87,st_size88,st_size89,st_size90, &
                   arrayst_size86,arrayst_size87,arrayst_size88,arrayst_size89,arrayst_size90, &
                   st_size91,st_size92,st_size93,st_size94,st_size95, &
                   arrayst_size91,arrayst_size92,arrayst_size93,arrayst_size94,arrayst_size95
 integer,optional,intent(in)           ::   &
                    lowb1, lowb2, lowb3, lowb4, lowb5, &
                    ilowb1, ilowb2, ilowb3, ilowb4, ilowb5, &
                    jlowb1, jlowb2, jlowb3, jlowb4, jlowb5, &
                    lowb6, lowb7, lowb8, lowb9, lowb10, &
                    ilowb6, ilowb7, ilowb8, ilowb9, ilowb10, &
                    jlowb6, jlowb7, jlowb8, jlowb9, jlowb10, &
                   lowb11,lowb12,lowb13,lowb14,lowb15, &
                   ilowb11,ilowb12,ilowb13,ilowb14,ilowb15, &
                   jlowb11,jlowb12,jlowb13,jlowb14,jlowb15, &
                   lowb16,lowb17,lowb18,lowb19,lowb20, &
                   ilowb16,ilowb17,ilowb18,ilowb19,ilowb20, &
                   jlowb16,jlowb17,jlowb18,jlowb19,jlowb20, &
                   lowb21,lowb22,lowb23,lowb24,lowb25, &
                   lowb26,lowb27,lowb28,lowb29,lowb30, &
                   lowb31,lowb32,lowb33,lowb34,lowb35, &
                   lowb36,lowb37,lowb38,lowb39,lowb40, &
                   lowb41,lowb42,lowb43,lowb44,lowb45, &
                   lowb46,lowb47,lowb48,lowb49,lowb50, &
                   lowb51,lowb52,lowb53,lowb54,lowb55, &
                   lowb56,lowb57,lowb58,lowb59,lowb60, &
                   lowb61,lowb62,lowb63,lowb64,lowb65, &
                   lowb66,lowb67,lowb68,lowb69,lowb70, &
                   lowb71,lowb72,lowb73,lowb74,lowb75, &
                   lowb76,lowb77,lowb78,lowb79,lowb80, &
                   lowb81,lowb82,lowb83,lowb84,lowb85, &
                   lowb86,lowb87,lowb88,lowb89,lowb90, &
                   lowb91,lowb92,lowb93,lowb94,lowb95
  !> Set up output JSON for this object 
  call cjson_write_json_object (nmlname)  
  if (present(var1 ) ) then
      if (.not. present (varname1 ) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var1 ,nmlname=nmlname, &
          varname=varname1 ,objname=objname1 ,st_size=st_size1 )
  endif
 !
  if (present(array1 ) ) then
      if (.not. present (arrayname1 ) ) call missing_name(nmlname)
     call json_call(cjson,json,array1 ,nmlname, &
          arrayname1 ,lowb1 ,arrayobj1 ,&
         arrayst_size1 )
  endif
 !
  if (present(a2d1 ) ) then
      if (.not. present (a2dname1 ) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d1 ,nmlname, &
          a2dname1 ,ilowb1 ,jlowb1 , &
          a2dobj1 ,a2dst_size1 )
  endif
  if (present(var2 ) ) then
      if (.not. present (varname2 ) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var2 ,nmlname=nmlname, &
          varname=varname2 ,objname=objname2 ,st_size=st_size2 )
  endif
 !
  if (present(array2 ) ) then
      if (.not. present (arrayname2 ) ) call missing_name(nmlname)
     call json_call(cjson,json,array2 ,nmlname, &
          arrayname2 ,lowb2 ,arrayobj2 ,&
         arrayst_size2 )
  endif
 !
  if (present(a2d2 ) ) then
      if (.not. present (a2dname2 ) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d2 ,nmlname, &
          a2dname2 ,ilowb2 ,jlowb2 , &
          a2dobj2 ,a2dst_size2 )
  endif
  if (present(var3 ) ) then
      if (.not. present (varname3 ) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var3 ,nmlname=nmlname, &
          varname=varname3 ,objname=objname3 ,st_size=st_size3 )
  endif
 !
  if (present(array3 ) ) then
      if (.not. present (arrayname3 ) ) call missing_name(nmlname)
     call json_call(cjson,json,array3 ,nmlname, &
          arrayname3 ,lowb3 ,arrayobj3 ,&
         arrayst_size3 )
  endif
 !
  if (present(a2d3 ) ) then
      if (.not. present (a2dname3 ) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d3 ,nmlname, &
          a2dname3 ,ilowb3 ,jlowb3 , &
          a2dobj3 ,a2dst_size3 )
  endif
  if (present(var4 ) ) then
      if (.not. present (varname4 ) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var4 ,nmlname=nmlname, &
          varname=varname4 ,objname=objname4 ,st_size=st_size4 )
  endif
 !
  if (present(array4 ) ) then
      if (.not. present (arrayname4 ) ) call missing_name(nmlname)
     call json_call(cjson,json,array4 ,nmlname, &
          arrayname4 ,lowb4 ,arrayobj4 ,&
         arrayst_size4 )
  endif
 !
  if (present(a2d4 ) ) then
      if (.not. present (a2dname4 ) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d4 ,nmlname, &
          a2dname4 ,ilowb4 ,jlowb4 , &
          a2dobj4 ,a2dst_size4 )
  endif
  if (present(var5 ) ) then
      if (.not. present (varname5 ) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var5 ,nmlname=nmlname, &
          varname=varname5 ,objname=objname5 ,st_size=st_size5 )
  endif
 !
  if (present(array5 ) ) then
      if (.not. present (arrayname5 ) ) call missing_name(nmlname)
     call json_call(cjson,json,array5 ,nmlname, &
          arrayname5 ,lowb5 ,arrayobj5 ,&
         arrayst_size5 )
  endif
 !
  if (present(a2d5 ) ) then
      if (.not. present (a2dname5 ) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d5 ,nmlname, &
          a2dname5 ,ilowb5 ,jlowb5 , &
          a2dobj5 ,a2dst_size5 )
  endif
  if (present(var6 ) ) then
      if (.not. present (varname6 ) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var6 ,nmlname=nmlname, &
          varname=varname6 ,objname=objname6 ,st_size=st_size6 )
  endif
 !
  if (present(array6 ) ) then
      if (.not. present (arrayname6 ) ) call missing_name(nmlname)
     call json_call(cjson,json,array6 ,nmlname, &
          arrayname6 ,lowb6 ,arrayobj6 ,&
         arrayst_size6 )
  endif
 !
  if (present(a2d6 ) ) then
      if (.not. present (a2dname6 ) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d6 ,nmlname, &
          a2dname6 ,ilowb6 ,jlowb6 , &
          a2dobj6 ,a2dst_size6 )
  endif
  if (present(var7 ) ) then
      if (.not. present (varname7 ) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var7 ,nmlname=nmlname, &
          varname=varname7 ,objname=objname7 ,st_size=st_size7 )
  endif
 !
  if (present(array7 ) ) then
      if (.not. present (arrayname7 ) ) call missing_name(nmlname)
     call json_call(cjson,json,array7 ,nmlname, &
          arrayname7 ,lowb7 ,arrayobj7 ,&
         arrayst_size7 )
  endif
 !
  if (present(a2d7 ) ) then
      if (.not. present (a2dname7 ) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d7 ,nmlname, &
          a2dname7 ,ilowb7 ,jlowb7 , &
          a2dobj7 ,a2dst_size7 )
  endif
  if (present(var8 ) ) then
      if (.not. present (varname8 ) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var8 ,nmlname=nmlname, &
          varname=varname8 ,objname=objname8 ,st_size=st_size8 )
  endif
 !
  if (present(array8 ) ) then
      if (.not. present (arrayname8 ) ) call missing_name(nmlname)
     call json_call(cjson,json,array8 ,nmlname, &
          arrayname8 ,lowb8 ,arrayobj8 ,&
         arrayst_size8 )
  endif
 !
  if (present(a2d8 ) ) then
      if (.not. present (a2dname8 ) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d8 ,nmlname, &
          a2dname8 ,ilowb8 ,jlowb8 , &
          a2dobj8 ,a2dst_size8 )
  endif
  if (present(var9 ) ) then
      if (.not. present (varname9 ) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var9 ,nmlname=nmlname, &
          varname=varname9 ,objname=objname9 ,st_size=st_size9 )
  endif
 !
  if (present(array9 ) ) then
      if (.not. present (arrayname9 ) ) call missing_name(nmlname)
     call json_call(cjson,json,array9 ,nmlname, &
          arrayname9 ,lowb9 ,arrayobj9 ,&
         arrayst_size9 )
  endif
 !
  if (present(a2d9 ) ) then
      if (.not. present (a2dname9 ) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d9 ,nmlname, &
          a2dname9 ,ilowb9 ,jlowb9 , &
          a2dobj9 ,a2dst_size9 )
  endif
  if (present(var10) ) then
      if (.not. present (varname10) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var10,nmlname=nmlname, &
          varname=varname10,objname=objname10,st_size=st_size10)
  endif
 !
  if (present(array10) ) then
      if (.not. present (arrayname10) ) call missing_name(nmlname)
     call json_call(cjson,json,array10,nmlname, &
          arrayname10,lowb10,arrayobj10,&
         arrayst_size10)
  endif
 !
  if (present(a2d10) ) then
      if (.not. present (a2dname10) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d10,nmlname, &
          a2dname10,ilowb10,jlowb10, &
          a2dobj10,a2dst_size10)
  endif
  if (present(var11) ) then
      if (.not. present (varname11) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var11,nmlname=nmlname, &
          varname=varname11,objname=objname11,st_size=st_size11)
  endif
 !
  if (present(array11) ) then
      if (.not. present (arrayname11) ) call missing_name(nmlname)
     call json_call(cjson,json,array11,nmlname, &
          arrayname11,lowb11,arrayobj11,&
         arrayst_size11)
  endif
 !
  if (present(a2d11) ) then
      if (.not. present (a2dname11) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d11,nmlname, &
          a2dname11,ilowb11,jlowb11, &
          a2dobj11,a2dst_size11)
  endif
  if (present(var12) ) then
      if (.not. present (varname12) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var12,nmlname=nmlname, &
          varname=varname12,objname=objname12,st_size=st_size12)
  endif
 !
  if (present(array12) ) then
      if (.not. present (arrayname12) ) call missing_name(nmlname)
     call json_call(cjson,json,array12,nmlname, &
          arrayname12,lowb12,arrayobj12,&
         arrayst_size12)
  endif
 !
  if (present(a2d12) ) then
      if (.not. present (a2dname12) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d12,nmlname, &
          a2dname12,ilowb12,jlowb12, &
          a2dobj12,a2dst_size12)
  endif
  if (present(var13) ) then
      if (.not. present (varname13) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var13,nmlname=nmlname, &
          varname=varname13,objname=objname13,st_size=st_size13)
  endif
 !
  if (present(array13) ) then
      if (.not. present (arrayname13) ) call missing_name(nmlname)
     call json_call(cjson,json,array13,nmlname, &
          arrayname13,lowb13,arrayobj13,&
         arrayst_size13)
  endif
 !
  if (present(a2d13) ) then
      if (.not. present (a2dname13) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d13,nmlname, &
          a2dname13,ilowb13,jlowb13, &
          a2dobj13,a2dst_size13)
  endif
  if (present(var14) ) then
      if (.not. present (varname14) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var14,nmlname=nmlname, &
          varname=varname14,objname=objname14,st_size=st_size14)
  endif
 !
  if (present(array14) ) then
      if (.not. present (arrayname14) ) call missing_name(nmlname)
     call json_call(cjson,json,array14,nmlname, &
          arrayname14,lowb14,arrayobj14,&
         arrayst_size14)
  endif
 !
  if (present(a2d14) ) then
      if (.not. present (a2dname14) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d14,nmlname, &
          a2dname14,ilowb14,jlowb14, &
          a2dobj14,a2dst_size14)
  endif
  if (present(var15) ) then
      if (.not. present (varname15) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var15,nmlname=nmlname, &
          varname=varname15,objname=objname15,st_size=st_size15)
  endif
 !
  if (present(array15) ) then
      if (.not. present (arrayname15) ) call missing_name(nmlname)
     call json_call(cjson,json,array15,nmlname, &
          arrayname15,lowb15,arrayobj15,&
         arrayst_size15)
  endif
 !
  if (present(a2d15) ) then
      if (.not. present (a2dname15) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d15,nmlname, &
          a2dname15,ilowb15,jlowb15, &
          a2dobj15,a2dst_size15)
  endif
  if (present(var16) ) then
      if (.not. present (varname16) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var16,nmlname=nmlname, &
          varname=varname16,objname=objname16,st_size=st_size16)
  endif
 !
  if (present(array16) ) then
      if (.not. present (arrayname16) ) call missing_name(nmlname)
     call json_call(cjson,json,array16,nmlname, &
          arrayname16,lowb16,arrayobj16,&
         arrayst_size16)
  endif
 !
  if (present(a2d16) ) then
      if (.not. present (a2dname16) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d16,nmlname, &
          a2dname16,ilowb16,jlowb16, &
          a2dobj16,a2dst_size16)
  endif
  if (present(var17) ) then
      if (.not. present (varname17) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var17,nmlname=nmlname, &
          varname=varname17,objname=objname17,st_size=st_size17)
  endif
 !
  if (present(array17) ) then
      if (.not. present (arrayname17) ) call missing_name(nmlname)
     call json_call(cjson,json,array17,nmlname, &
          arrayname17,lowb17,arrayobj17,&
         arrayst_size17)
  endif
 !
  if (present(a2d17) ) then
      if (.not. present (a2dname17) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d17,nmlname, &
          a2dname17,ilowb17,jlowb17, &
          a2dobj17,a2dst_size17)
  endif
  if (present(var18) ) then
      if (.not. present (varname18) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var18,nmlname=nmlname, &
          varname=varname18,objname=objname18,st_size=st_size18)
  endif
 !
  if (present(array18) ) then
      if (.not. present (arrayname18) ) call missing_name(nmlname)
     call json_call(cjson,json,array18,nmlname, &
          arrayname18,lowb18,arrayobj18,&
         arrayst_size18)
  endif
 !
  if (present(a2d18) ) then
      if (.not. present (a2dname18) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d18,nmlname, &
          a2dname18,ilowb18,jlowb18, &
          a2dobj18,a2dst_size18)
  endif
  if (present(var19) ) then
      if (.not. present (varname19) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var19,nmlname=nmlname, &
          varname=varname19,objname=objname19,st_size=st_size19)
  endif
 !
  if (present(array19) ) then
      if (.not. present (arrayname19) ) call missing_name(nmlname)
     call json_call(cjson,json,array19,nmlname, &
          arrayname19,lowb19,arrayobj19,&
         arrayst_size19)
  endif
 !
  if (present(a2d19) ) then
      if (.not. present (a2dname19) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d19,nmlname, &
          a2dname19,ilowb19,jlowb19, &
          a2dobj19,a2dst_size19)
  endif
  if (present(var20) ) then
      if (.not. present (varname20) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var20,nmlname=nmlname, &
          varname=varname20,objname=objname20,st_size=st_size20)
  endif
 !
  if (present(array20) ) then
      if (.not. present (arrayname20) ) call missing_name(nmlname)
     call json_call(cjson,json,array20,nmlname, &
          arrayname20,lowb20,arrayobj20,&
         arrayst_size20)
  endif
 !
  if (present(a2d20) ) then
      if (.not. present (a2dname20) ) call missing_name(nmlname)
     call json_call(cjson,json,a2d20,nmlname, &
          a2dname20,ilowb20,jlowb20, &
          a2dobj20,a2dst_size20)
  endif
  if (present(var21) ) then
      if (.not. present (varname21) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var21,nmlname=nmlname, &
          varname=varname21,objname=objname21,st_size=st_size21)
  endif
 !
  if (present(array21) ) then
      if (.not. present (arrayname21) ) call missing_name(nmlname)
     call json_call(cjson,json,array21,nmlname, &
          arrayname21,lowb21,arrayobj21,&
         arrayst_size21)
  endif
 !
  if (present(var22) ) then
      if (.not. present (varname22) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var22,nmlname=nmlname, &
          varname=varname22,objname=objname22,st_size=st_size22)
  endif
 !
  if (present(array22) ) then
      if (.not. present (arrayname22) ) call missing_name(nmlname)
     call json_call(cjson,json,array22,nmlname, &
          arrayname22,lowb22,arrayobj22,&
         arrayst_size22)
  endif
 !
  if (present(var23) ) then
      if (.not. present (varname23) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var23,nmlname=nmlname, &
          varname=varname23,objname=objname23,st_size=st_size23)
  endif
 !
  if (present(array23) ) then
      if (.not. present (arrayname23) ) call missing_name(nmlname)
     call json_call(cjson,json,array23,nmlname, &
          arrayname23,lowb23,arrayobj23,&
         arrayst_size23)
  endif
 !
  if (present(var24) ) then
      if (.not. present (varname24) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var24,nmlname=nmlname, &
          varname=varname24,objname=objname24,st_size=st_size24)
  endif
 !
  if (present(array24) ) then
      if (.not. present (arrayname24) ) call missing_name(nmlname)
     call json_call(cjson,json,array24,nmlname, &
          arrayname24,lowb24,arrayobj24,&
         arrayst_size24)
  endif
 !
  if (present(var25) ) then
      if (.not. present (varname25) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var25,nmlname=nmlname, &
          varname=varname25,objname=objname25,st_size=st_size25)
  endif
 !
  if (present(array25) ) then
      if (.not. present (arrayname25) ) call missing_name(nmlname)
     call json_call(cjson,json,array25,nmlname, &
          arrayname25,lowb25,arrayobj25,&
         arrayst_size25)
  endif
 !
  if (present(var26) ) then
      if (.not. present (varname26) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var26,nmlname=nmlname, &
          varname=varname26,objname=objname26,st_size=st_size26)
  endif
 !
  if (present(array26) ) then
      if (.not. present (arrayname26) ) call missing_name(nmlname)
     call json_call(cjson,json,array26,nmlname, &
          arrayname26,lowb26,arrayobj26,&
         arrayst_size26)
  endif
 !
  if (present(var27) ) then
      if (.not. present (varname27) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var27,nmlname=nmlname, &
          varname=varname27,objname=objname27,st_size=st_size27)
  endif
 !
  if (present(array27) ) then
      if (.not. present (arrayname27) ) call missing_name(nmlname)
     call json_call(cjson,json,array27,nmlname, &
          arrayname27,lowb27,arrayobj27,&
         arrayst_size27)
  endif
 !
  if (present(var28) ) then
      if (.not. present (varname28) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var28,nmlname=nmlname, &
          varname=varname28,objname=objname28,st_size=st_size28)
  endif
 !
  if (present(array28) ) then
      if (.not. present (arrayname28) ) call missing_name(nmlname)
     call json_call(cjson,json,array28,nmlname, &
          arrayname28,lowb28,arrayobj28,&
         arrayst_size28)
  endif
 !
  if (present(var29) ) then
      if (.not. present (varname29) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var29,nmlname=nmlname, &
          varname=varname29,objname=objname29,st_size=st_size29)
  endif
 !
  if (present(array29) ) then
      if (.not. present (arrayname29) ) call missing_name(nmlname)
     call json_call(cjson,json,array29,nmlname, &
          arrayname29,lowb29,arrayobj29,&
         arrayst_size29)
  endif
 !
  if (present(var30) ) then
      if (.not. present (varname30) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var30,nmlname=nmlname, &
          varname=varname30,objname=objname30,st_size=st_size30)
  endif
 !
  if (present(array30) ) then
      if (.not. present (arrayname30) ) call missing_name(nmlname)
     call json_call(cjson,json,array30,nmlname, &
          arrayname30,lowb30,arrayobj30,&
         arrayst_size30)
  endif
 !
  if (present(var31) ) then
      if (.not. present (varname31) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var31,nmlname=nmlname, &
          varname=varname31,objname=objname31,st_size=st_size31)
  endif
 !
  if (present(array31) ) then
      if (.not. present (arrayname31) ) call missing_name(nmlname)
     call json_call(cjson,json,array31,nmlname, &
          arrayname31,lowb31,arrayobj31,&
         arrayst_size31)
  endif
 !
  if (present(var32) ) then
      if (.not. present (varname32) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var32,nmlname=nmlname, &
          varname=varname32,objname=objname32,st_size=st_size32)
  endif
 !
  if (present(array32) ) then
      if (.not. present (arrayname32) ) call missing_name(nmlname)
     call json_call(cjson,json,array32,nmlname, &
          arrayname32,lowb32,arrayobj32,&
         arrayst_size32)
  endif
 !
  if (present(var33) ) then
      if (.not. present (varname33) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var33,nmlname=nmlname, &
          varname=varname33,objname=objname33,st_size=st_size33)
  endif
 !
  if (present(array33) ) then
      if (.not. present (arrayname33) ) call missing_name(nmlname)
     call json_call(cjson,json,array33,nmlname, &
          arrayname33,lowb33,arrayobj33,&
         arrayst_size33)
  endif
 !
  if (present(var34) ) then
      if (.not. present (varname34) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var34,nmlname=nmlname, &
          varname=varname34,objname=objname34,st_size=st_size34)
  endif
 !
  if (present(array34) ) then
      if (.not. present (arrayname34) ) call missing_name(nmlname)
     call json_call(cjson,json,array34,nmlname, &
          arrayname34,lowb34,arrayobj34,&
         arrayst_size34)
  endif
 !
  if (present(var35) ) then
      if (.not. present (varname35) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var35,nmlname=nmlname, &
          varname=varname35,objname=objname35,st_size=st_size35)
  endif
 !
  if (present(array35) ) then
      if (.not. present (arrayname35) ) call missing_name(nmlname)
     call json_call(cjson,json,array35,nmlname, &
          arrayname35,lowb35,arrayobj35,&
         arrayst_size35)
  endif
 !
  if (present(var36) ) then
      if (.not. present (varname36) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var36,nmlname=nmlname, &
          varname=varname36,objname=objname36,st_size=st_size36)
  endif
 !
  if (present(array36) ) then
      if (.not. present (arrayname36) ) call missing_name(nmlname)
     call json_call(cjson,json,array36,nmlname, &
          arrayname36,lowb36,arrayobj36,&
         arrayst_size36)
  endif
 !
  if (present(var37) ) then
      if (.not. present (varname37) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var37,nmlname=nmlname, &
          varname=varname37,objname=objname37,st_size=st_size37)
  endif
 !
  if (present(array37) ) then
      if (.not. present (arrayname37) ) call missing_name(nmlname)
     call json_call(cjson,json,array37,nmlname, &
          arrayname37,lowb37,arrayobj37,&
         arrayst_size37)
  endif
 !
  if (present(var38) ) then
      if (.not. present (varname38) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var38,nmlname=nmlname, &
          varname=varname38,objname=objname38,st_size=st_size38)
  endif
 !
  if (present(array38) ) then
      if (.not. present (arrayname38) ) call missing_name(nmlname)
     call json_call(cjson,json,array38,nmlname, &
          arrayname38,lowb38,arrayobj38,&
         arrayst_size38)
  endif
 !
  if (present(var39) ) then
      if (.not. present (varname39) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var39,nmlname=nmlname, &
          varname=varname39,objname=objname39,st_size=st_size39)
  endif
 !
  if (present(array39) ) then
      if (.not. present (arrayname39) ) call missing_name(nmlname)
     call json_call(cjson,json,array39,nmlname, &
          arrayname39,lowb39,arrayobj39,&
         arrayst_size39)
  endif
 !
  if (present(var40) ) then
      if (.not. present (varname40) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var40,nmlname=nmlname, &
          varname=varname40,objname=objname40,st_size=st_size40)
  endif
 !
  if (present(array40) ) then
      if (.not. present (arrayname40) ) call missing_name(nmlname)
     call json_call(cjson,json,array40,nmlname, &
          arrayname40,lowb40,arrayobj40,&
         arrayst_size40)
  endif
 !
  if (present(var41) ) then
      if (.not. present (varname41) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var41,nmlname=nmlname, &
          varname=varname41,objname=objname41,st_size=st_size41)
  endif
 !
  if (present(array41) ) then
      if (.not. present (arrayname41) ) call missing_name(nmlname)
     call json_call(cjson,json,array41,nmlname, &
          arrayname41,lowb41,arrayobj41,&
         arrayst_size41)
  endif
 !
  if (present(var42) ) then
      if (.not. present (varname42) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var42,nmlname=nmlname, &
          varname=varname42,objname=objname42,st_size=st_size42)
  endif
 !
  if (present(array42) ) then
      if (.not. present (arrayname42) ) call missing_name(nmlname)
     call json_call(cjson,json,array42,nmlname, &
          arrayname42,lowb42,arrayobj42,&
         arrayst_size42)
  endif
 !
  if (present(var43) ) then
      if (.not. present (varname43) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var43,nmlname=nmlname, &
          varname=varname43,objname=objname43,st_size=st_size43)
  endif
 !
  if (present(array43) ) then
      if (.not. present (arrayname43) ) call missing_name(nmlname)
     call json_call(cjson,json,array43,nmlname, &
          arrayname43,lowb43,arrayobj43,&
         arrayst_size43)
  endif
 !
  if (present(var44) ) then
      if (.not. present (varname44) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var44,nmlname=nmlname, &
          varname=varname44,objname=objname44,st_size=st_size44)
  endif
 !
  if (present(array44) ) then
      if (.not. present (arrayname44) ) call missing_name(nmlname)
     call json_call(cjson,json,array44,nmlname, &
          arrayname44,lowb44,arrayobj44,&
         arrayst_size44)
  endif
 !
  if (present(var45) ) then
      if (.not. present (varname45) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var45,nmlname=nmlname, &
          varname=varname45,objname=objname45,st_size=st_size45)
  endif
 !
  if (present(array45) ) then
      if (.not. present (arrayname45) ) call missing_name(nmlname)
     call json_call(cjson,json,array45,nmlname, &
          arrayname45,lowb45,arrayobj45,&
         arrayst_size45)
  endif
 !
  if (present(var46) ) then
      if (.not. present (varname46) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var46,nmlname=nmlname, &
          varname=varname46,objname=objname46,st_size=st_size46)
  endif
 !
  if (present(array46) ) then
      if (.not. present (arrayname46) ) call missing_name(nmlname)
     call json_call(cjson,json,array46,nmlname, &
          arrayname46,lowb46,arrayobj46,&
         arrayst_size46)
  endif
 !
  if (present(var47) ) then
      if (.not. present (varname47) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var47,nmlname=nmlname, &
          varname=varname47,objname=objname47,st_size=st_size47)
  endif
 !
  if (present(array47) ) then
      if (.not. present (arrayname47) ) call missing_name(nmlname)
     call json_call(cjson,json,array47,nmlname, &
          arrayname47,lowb47,arrayobj47,&
         arrayst_size47)
  endif
 !
  if (present(var48) ) then
      if (.not. present (varname48) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var48,nmlname=nmlname, &
          varname=varname48,objname=objname48,st_size=st_size48)
  endif
 !
  if (present(array48) ) then
      if (.not. present (arrayname48) ) call missing_name(nmlname)
     call json_call(cjson,json,array48,nmlname, &
          arrayname48,lowb48,arrayobj48,&
         arrayst_size48)
  endif
 !
  if (present(var49) ) then
      if (.not. present (varname49) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var49,nmlname=nmlname, &
          varname=varname49,objname=objname49,st_size=st_size49)
  endif
 !
  if (present(array49) ) then
      if (.not. present (arrayname49) ) call missing_name(nmlname)
     call json_call(cjson,json,array49,nmlname, &
          arrayname49,lowb49,arrayobj49,&
         arrayst_size49)
  endif
 !
  if (present(var50) ) then
      if (.not. present (varname50) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var50,nmlname=nmlname, &
          varname=varname50,objname=objname50,st_size=st_size50)
  endif
 !
  if (present(array50) ) then
      if (.not. present (arrayname50) ) call missing_name(nmlname)
     call json_call(cjson,json,array50,nmlname, &
          arrayname50,lowb50,arrayobj50,&
         arrayst_size50)
  endif
 !
  if (present(var51) ) then
      if (.not. present (varname51) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var51,nmlname=nmlname, &
          varname=varname51,objname=objname51,st_size=st_size51)
  endif
 !
  if (present(array51) ) then
      if (.not. present (arrayname51) ) call missing_name(nmlname)
     call json_call(cjson,json,array51,nmlname, &
          arrayname51,lowb51,arrayobj51,&
         arrayst_size51)
  endif
 !
  if (present(var52) ) then
      if (.not. present (varname52) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var52,nmlname=nmlname, &
          varname=varname52,objname=objname52,st_size=st_size52)
  endif
 !
  if (present(array52) ) then
      if (.not. present (arrayname52) ) call missing_name(nmlname)
     call json_call(cjson,json,array52,nmlname, &
          arrayname52,lowb52,arrayobj52,&
         arrayst_size52)
  endif
 !
  if (present(var53) ) then
      if (.not. present (varname53) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var53,nmlname=nmlname, &
          varname=varname53,objname=objname53,st_size=st_size53)
  endif
 !
  if (present(array53) ) then
      if (.not. present (arrayname53) ) call missing_name(nmlname)
     call json_call(cjson,json,array53,nmlname, &
          arrayname53,lowb53,arrayobj53,&
         arrayst_size53)
  endif
 !
  if (present(var54) ) then
      if (.not. present (varname54) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var54,nmlname=nmlname, &
          varname=varname54,objname=objname54,st_size=st_size54)
  endif
 !
  if (present(array54) ) then
      if (.not. present (arrayname54) ) call missing_name(nmlname)
     call json_call(cjson,json,array54,nmlname, &
          arrayname54,lowb54,arrayobj54,&
         arrayst_size54)
  endif
 !
  if (present(var55) ) then
      if (.not. present (varname55) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var55,nmlname=nmlname, &
          varname=varname55,objname=objname55,st_size=st_size55)
  endif
 !
  if (present(array55) ) then
      if (.not. present (arrayname55) ) call missing_name(nmlname)
     call json_call(cjson,json,array55,nmlname, &
          arrayname55,lowb55,arrayobj55,&
         arrayst_size55)
  endif
 !
  if (present(var56) ) then
      if (.not. present (varname56) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var56,nmlname=nmlname, &
          varname=varname56,objname=objname56,st_size=st_size56)
  endif
 !
  if (present(array56) ) then
      if (.not. present (arrayname56) ) call missing_name(nmlname)
     call json_call(cjson,json,array56,nmlname, &
          arrayname56,lowb56,arrayobj56,&
         arrayst_size56)
  endif
 !
  if (present(var57) ) then
      if (.not. present (varname57) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var57,nmlname=nmlname, &
          varname=varname57,objname=objname57,st_size=st_size57)
  endif
 !
  if (present(array57) ) then
      if (.not. present (arrayname57) ) call missing_name(nmlname)
     call json_call(cjson,json,array57,nmlname, &
          arrayname57,lowb57,arrayobj57,&
         arrayst_size57)
  endif
 !
  if (present(var58) ) then
      if (.not. present (varname58) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var58,nmlname=nmlname, &
          varname=varname58,objname=objname58,st_size=st_size58)
  endif
 !
  if (present(array58) ) then
      if (.not. present (arrayname58) ) call missing_name(nmlname)
     call json_call(cjson,json,array58,nmlname, &
          arrayname58,lowb58,arrayobj58,&
         arrayst_size58)
  endif
 !
  if (present(var59) ) then
      if (.not. present (varname59) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var59,nmlname=nmlname, &
          varname=varname59,objname=objname59,st_size=st_size59)
  endif
 !
  if (present(array59) ) then
      if (.not. present (arrayname59) ) call missing_name(nmlname)
     call json_call(cjson,json,array59,nmlname, &
          arrayname59,lowb59,arrayobj59,&
         arrayst_size59)
  endif
 !
  if (present(var60) ) then
      if (.not. present (varname60) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var60,nmlname=nmlname, &
          varname=varname60,objname=objname60,st_size=st_size60)
  endif
 !
  if (present(array60) ) then
      if (.not. present (arrayname60) ) call missing_name(nmlname)
     call json_call(cjson,json,array60,nmlname, &
          arrayname60,lowb60,arrayobj60,&
         arrayst_size60)
  endif
 !
  if (present(var61) ) then
      if (.not. present (varname61) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var61,nmlname=nmlname, &
          varname=varname61,objname=objname61,st_size=st_size61)
  endif
 !
  if (present(array61) ) then
      if (.not. present (arrayname61) ) call missing_name(nmlname)
     call json_call(cjson,json,array61,nmlname, &
          arrayname61,lowb61,arrayobj61,&
         arrayst_size61)
  endif
 !
  if (present(var62) ) then
      if (.not. present (varname62) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var62,nmlname=nmlname, &
          varname=varname62,objname=objname62,st_size=st_size62)
  endif
 !
  if (present(array62) ) then
      if (.not. present (arrayname62) ) call missing_name(nmlname)
     call json_call(cjson,json,array62,nmlname, &
          arrayname62,lowb62,arrayobj62,&
         arrayst_size62)
  endif
 !
  if (present(var63) ) then
      if (.not. present (varname63) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var63,nmlname=nmlname, &
          varname=varname63,objname=objname63,st_size=st_size63)
  endif
 !
  if (present(array63) ) then
      if (.not. present (arrayname63) ) call missing_name(nmlname)
     call json_call(cjson,json,array63,nmlname, &
          arrayname63,lowb63,arrayobj63,&
         arrayst_size63)
  endif
 !
  if (present(var64) ) then
      if (.not. present (varname64) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var64,nmlname=nmlname, &
          varname=varname64,objname=objname64,st_size=st_size64)
  endif
 !
  if (present(array64) ) then
      if (.not. present (arrayname64) ) call missing_name(nmlname)
     call json_call(cjson,json,array64,nmlname, &
          arrayname64,lowb64,arrayobj64,&
         arrayst_size64)
  endif
 !
  if (present(var65) ) then
      if (.not. present (varname65) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var65,nmlname=nmlname, &
          varname=varname65,objname=objname65,st_size=st_size65)
  endif
 !
  if (present(array65) ) then
      if (.not. present (arrayname65) ) call missing_name(nmlname)
     call json_call(cjson,json,array65,nmlname, &
          arrayname65,lowb65,arrayobj65,&
         arrayst_size65)
  endif
 !
  if (present(var66) ) then
      if (.not. present (varname66) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var66,nmlname=nmlname, &
          varname=varname66,objname=objname66,st_size=st_size66)
  endif
 !
  if (present(array66) ) then
      if (.not. present (arrayname66) ) call missing_name(nmlname)
     call json_call(cjson,json,array66,nmlname, &
          arrayname66,lowb66,arrayobj66,&
         arrayst_size66)
  endif
 !
  if (present(var67) ) then
      if (.not. present (varname67) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var67,nmlname=nmlname, &
          varname=varname67,objname=objname67,st_size=st_size67)
  endif
 !
  if (present(array67) ) then
      if (.not. present (arrayname67) ) call missing_name(nmlname)
     call json_call(cjson,json,array67,nmlname, &
          arrayname67,lowb67,arrayobj67,&
         arrayst_size67)
  endif
 !
  if (present(var68) ) then
      if (.not. present (varname68) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var68,nmlname=nmlname, &
          varname=varname68,objname=objname68,st_size=st_size68)
  endif
 !
  if (present(array68) ) then
      if (.not. present (arrayname68) ) call missing_name(nmlname)
     call json_call(cjson,json,array68,nmlname, &
          arrayname68,lowb68,arrayobj68,&
         arrayst_size68)
  endif
 !
  if (present(var69) ) then
      if (.not. present (varname69) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var69,nmlname=nmlname, &
          varname=varname69,objname=objname69,st_size=st_size69)
  endif
 !
  if (present(array69) ) then
      if (.not. present (arrayname69) ) call missing_name(nmlname)
     call json_call(cjson,json,array69,nmlname, &
          arrayname69,lowb69,arrayobj69,&
         arrayst_size69)
  endif
 !
  if (present(var70) ) then
      if (.not. present (varname70) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var70,nmlname=nmlname, &
          varname=varname70,objname=objname70,st_size=st_size70)
  endif
 !
  if (present(array70) ) then
      if (.not. present (arrayname70) ) call missing_name(nmlname)
     call json_call(cjson,json,array70,nmlname, &
          arrayname70,lowb70,arrayobj70,&
         arrayst_size70)
  endif
 !
  if (present(var71) ) then
      if (.not. present (varname71) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var71,nmlname=nmlname, &
          varname=varname71,objname=objname71,st_size=st_size71)
  endif
 !
  if (present(array71) ) then
      if (.not. present (arrayname71) ) call missing_name(nmlname)
     call json_call(cjson,json,array71,nmlname, &
          arrayname71,lowb71,arrayobj71,&
         arrayst_size71)
  endif
 !
  if (present(var72) ) then
      if (.not. present (varname72) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var72,nmlname=nmlname, &
          varname=varname72,objname=objname72,st_size=st_size72)
  endif
 !
  if (present(array72) ) then
      if (.not. present (arrayname72) ) call missing_name(nmlname)
     call json_call(cjson,json,array72,nmlname, &
          arrayname72,lowb72,arrayobj72,&
         arrayst_size72)
  endif
 !
  if (present(var73) ) then
      if (.not. present (varname73) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var73,nmlname=nmlname, &
          varname=varname73,objname=objname73,st_size=st_size73)
  endif
 !
  if (present(array73) ) then
      if (.not. present (arrayname73) ) call missing_name(nmlname)
     call json_call(cjson,json,array73,nmlname, &
          arrayname73,lowb73,arrayobj73,&
         arrayst_size73)
  endif
 !
  if (present(var74) ) then
      if (.not. present (varname74) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var74,nmlname=nmlname, &
          varname=varname74,objname=objname74,st_size=st_size74)
  endif
 !
  if (present(array74) ) then
      if (.not. present (arrayname74) ) call missing_name(nmlname)
     call json_call(cjson,json,array74,nmlname, &
          arrayname74,lowb74,arrayobj74,&
         arrayst_size74)
  endif
 !
  if (present(var75) ) then
      if (.not. present (varname75) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var75,nmlname=nmlname, &
          varname=varname75,objname=objname75,st_size=st_size75)
  endif
 !
  if (present(array75) ) then
      if (.not. present (arrayname75) ) call missing_name(nmlname)
     call json_call(cjson,json,array75,nmlname, &
          arrayname75,lowb75,arrayobj75,&
         arrayst_size75)
  endif
 !
  if (present(var76) ) then
      if (.not. present (varname76) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var76,nmlname=nmlname, &
          varname=varname76,objname=objname76,st_size=st_size76)
  endif
 !
  if (present(array76) ) then
      if (.not. present (arrayname76) ) call missing_name(nmlname)
     call json_call(cjson,json,array76,nmlname, &
          arrayname76,lowb76,arrayobj76,&
         arrayst_size76)
  endif
 !
  if (present(var77) ) then
      if (.not. present (varname77) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var77,nmlname=nmlname, &
          varname=varname77,objname=objname77,st_size=st_size77)
  endif
 !
  if (present(array77) ) then
      if (.not. present (arrayname77) ) call missing_name(nmlname)
     call json_call(cjson,json,array77,nmlname, &
          arrayname77,lowb77,arrayobj77,&
         arrayst_size77)
  endif
 !
  if (present(var78) ) then
      if (.not. present (varname78) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var78,nmlname=nmlname, &
          varname=varname78,objname=objname78,st_size=st_size78)
  endif
 !
  if (present(array78) ) then
      if (.not. present (arrayname78) ) call missing_name(nmlname)
     call json_call(cjson,json,array78,nmlname, &
          arrayname78,lowb78,arrayobj78,&
         arrayst_size78)
  endif
 !
  if (present(var79) ) then
      if (.not. present (varname79) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var79,nmlname=nmlname, &
          varname=varname79,objname=objname79,st_size=st_size79)
  endif
 !
  if (present(array79) ) then
      if (.not. present (arrayname79) ) call missing_name(nmlname)
     call json_call(cjson,json,array79,nmlname, &
          arrayname79,lowb79,arrayobj79,&
         arrayst_size79)
  endif
 !
  if (present(var80) ) then
      if (.not. present (varname80) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var80,nmlname=nmlname, &
          varname=varname80,objname=objname80,st_size=st_size80)
  endif
 !
  if (present(array80) ) then
      if (.not. present (arrayname80) ) call missing_name(nmlname)
     call json_call(cjson,json,array80,nmlname, &
          arrayname80,lowb80,arrayobj80,&
         arrayst_size80)
  endif
 !
  if (present(var81) ) then
      if (.not. present (varname81) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var81,nmlname=nmlname, &
          varname=varname81,objname=objname81,st_size=st_size81)
  endif
 !
  if (present(array81) ) then
      if (.not. present (arrayname81) ) call missing_name(nmlname)
     call json_call(cjson,json,array81,nmlname, &
          arrayname81,lowb81,arrayobj81,&
         arrayst_size81)
  endif
 !
  if (present(var82) ) then
      if (.not. present (varname82) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var82,nmlname=nmlname, &
          varname=varname82,objname=objname82,st_size=st_size82)
  endif
 !
  if (present(array82) ) then
      if (.not. present (arrayname82) ) call missing_name(nmlname)
     call json_call(cjson,json,array82,nmlname, &
          arrayname82,lowb82,arrayobj82,&
         arrayst_size82)
  endif
 !
  if (present(var83) ) then
      if (.not. present (varname83) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var83,nmlname=nmlname, &
          varname=varname83,objname=objname83,st_size=st_size83)
  endif
 !
  if (present(array83) ) then
      if (.not. present (arrayname83) ) call missing_name(nmlname)
     call json_call(cjson,json,array83,nmlname, &
          arrayname83,lowb83,arrayobj83,&
         arrayst_size83)
  endif
 !
  if (present(var84) ) then
      if (.not. present (varname84) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var84,nmlname=nmlname, &
          varname=varname84,objname=objname84,st_size=st_size84)
  endif
 !
  if (present(array84) ) then
      if (.not. present (arrayname84) ) call missing_name(nmlname)
     call json_call(cjson,json,array84,nmlname, &
          arrayname84,lowb84,arrayobj84,&
         arrayst_size84)
  endif
 !
  if (present(var85) ) then
      if (.not. present (varname85) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var85,nmlname=nmlname, &
          varname=varname85,objname=objname85,st_size=st_size85)
  endif
 !
  if (present(array85) ) then
      if (.not. present (arrayname85) ) call missing_name(nmlname)
     call json_call(cjson,json,array85,nmlname, &
          arrayname85,lowb85,arrayobj85,&
         arrayst_size85)
  endif
 !
  if (present(var86) ) then
      if (.not. present (varname86) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var86,nmlname=nmlname, &
          varname=varname86,objname=objname86,st_size=st_size86)
  endif
 !
  if (present(array86) ) then
      if (.not. present (arrayname86) ) call missing_name(nmlname)
     call json_call(cjson,json,array86,nmlname, &
          arrayname86,lowb86,arrayobj86,&
         arrayst_size86)
  endif
 !
  if (present(var87) ) then
      if (.not. present (varname87) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var87,nmlname=nmlname, &
          varname=varname87,objname=objname87,st_size=st_size87)
  endif
 !
  if (present(array87) ) then
      if (.not. present (arrayname87) ) call missing_name(nmlname)
     call json_call(cjson,json,array87,nmlname, &
          arrayname87,lowb87,arrayobj87,&
         arrayst_size87)
  endif
 !
  if (present(var88) ) then
      if (.not. present (varname88) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var88,nmlname=nmlname, &
          varname=varname88,objname=objname88,st_size=st_size88)
  endif
 !
  if (present(array88) ) then
      if (.not. present (arrayname88) ) call missing_name(nmlname)
     call json_call(cjson,json,array88,nmlname, &
          arrayname88,lowb88,arrayobj88,&
         arrayst_size88)
  endif
 !
  if (present(var89) ) then
      if (.not. present (varname89) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var89,nmlname=nmlname, &
          varname=varname89,objname=objname89,st_size=st_size89)
  endif
 !
  if (present(array89) ) then
      if (.not. present (arrayname89) ) call missing_name(nmlname)
     call json_call(cjson,json,array89,nmlname, &
          arrayname89,lowb89,arrayobj89,&
         arrayst_size89)
  endif
 !
  if (present(var90) ) then
      if (.not. present (varname90) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var90,nmlname=nmlname, &
          varname=varname90,objname=objname90,st_size=st_size90)
  endif
 !
  if (present(array90) ) then
      if (.not. present (arrayname90) ) call missing_name(nmlname)
     call json_call(cjson,json,array90,nmlname, &
          arrayname90,lowb90,arrayobj90,&
         arrayst_size90)
  endif
 !
  if (present(var91) ) then
      if (.not. present (varname91) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var91,nmlname=nmlname, &
          varname=varname91,objname=objname91,st_size=st_size91)
  endif
 !
  if (present(array91) ) then
      if (.not. present (arrayname91) ) call missing_name(nmlname)
     call json_call(cjson,json,array91,nmlname, &
          arrayname91,lowb91,arrayobj91,&
         arrayst_size91)
  endif
 !
  if (present(var92) ) then
      if (.not. present (varname92) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var92,nmlname=nmlname, &
          varname=varname92,objname=objname92,st_size=st_size92)
  endif
 !
  if (present(array92) ) then
      if (.not. present (arrayname92) ) call missing_name(nmlname)
     call json_call(cjson,json,array92,nmlname, &
          arrayname92,lowb92,arrayobj92,&
         arrayst_size92)
  endif
 !
  if (present(var93) ) then
      if (.not. present (varname93) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var93,nmlname=nmlname, &
          varname=varname93,objname=objname93,st_size=st_size93)
  endif
 !
  if (present(array93) ) then
      if (.not. present (arrayname93) ) call missing_name(nmlname)
     call json_call(cjson,json,array93,nmlname, &
          arrayname93,lowb93,arrayobj93,&
         arrayst_size93)
  endif
 !
  if (present(var94) ) then
      if (.not. present (varname94) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var94,nmlname=nmlname, &
          varname=varname94,objname=objname94,st_size=st_size94)
  endif
 !
  if (present(array94) ) then
      if (.not. present (arrayname94) ) call missing_name(nmlname)
     call json_call(cjson,json,array94,nmlname, &
          arrayname94,lowb94,arrayobj94,&
         arrayst_size94)
  endif
 !
  if (present(var95) ) then
      if (.not. present (varname95) ) call missing_name(nmlname)
     call json_call(cjson=cjson,json=json,var=var95,nmlname=nmlname, &
          varname=varname95,objname=objname95,st_size=st_size95)
  endif
 !
  if (present(array95) ) then
      if (.not. present (arrayname95) ) call missing_name(nmlname)
     call json_call(cjson,json,array95,nmlname, &
          arrayname95,lowb95,arrayobj95,&
         arrayst_size95)
  endif
 !
  call check_json_for_stuff_doesnt_belong (json,nmlname,& 
 varname1, arrayname1, a2dname1, &
 varname2, arrayname2, a2dname2, &
 varname3, arrayname3, a2dname3, &
 varname4, arrayname4, a2dname4, &
 varname5, arrayname5, a2dname5, &
 varname6, arrayname6, a2dname6, &
 varname7, arrayname7, a2dname7, &
 varname8, arrayname8, a2dname8, &
 varname9, arrayname9, a2dname9, &
 varname10, arrayname10, a2dname10, &
 varname11, arrayname11, a2dname11, &
 varname12, arrayname12, a2dname12, &
 varname13, arrayname13, a2dname13, &
 varname14, arrayname14, a2dname14, &
 varname15, arrayname15, a2dname15, &
 varname16, arrayname16, a2dname16, &
 varname17, arrayname17, a2dname17, &
 varname18, arrayname18, a2dname18, &
 varname19, arrayname19, a2dname19, &
 varname20, arrayname20, a2dname20, &
 varname21, arrayname21, &
 varname22, arrayname22, &
 varname23, arrayname23, &
 varname24, arrayname24, &
 varname25, arrayname25, &
 varname26, arrayname26, &
 varname27, arrayname27, &
 varname28, arrayname28, &
 varname29, arrayname29, &
 varname30, arrayname30, &
 varname31, arrayname31, &
 varname32, arrayname32, &
 varname33, arrayname33, &
 varname34, arrayname34, &
 varname35, arrayname35, &
 varname36, arrayname36, &
 varname37, arrayname37, &
 varname38, arrayname38, &
 varname39, arrayname39, &
 varname40, arrayname40, &
 varname41, arrayname41, &
 varname42, arrayname42, &
 varname43, arrayname43, &
 varname44, arrayname44, &
 varname45, arrayname45, &
 varname46, arrayname46, &
 varname47, arrayname47, &
 varname48, arrayname48, &
 varname49, arrayname49, &
 varname50, arrayname50, &
 varname51, arrayname51, &
 varname52, arrayname52, &
 varname53, arrayname53, &
 varname54, arrayname54, &
 varname55, arrayname55, &
 varname56, arrayname56, &
 varname57, arrayname57, &
 varname58, arrayname58, &
 varname59, arrayname59, &
 varname60, arrayname60, &
 varname61, arrayname61, &
 varname62, arrayname62, &
 varname63, arrayname63, &
 varname64, arrayname64, &
 varname65, arrayname65, &
 varname66, arrayname66, &
 varname67, arrayname67, &
 varname68, arrayname68, &
 varname69, arrayname69, &
 varname70, arrayname70, &
 varname71, arrayname71, &
 varname72, arrayname72, &
 varname73, arrayname73, &
 varname74, arrayname74, &
 varname75, arrayname75, &
 varname76, arrayname76, &
 varname77, arrayname77, &
 varname78, arrayname78, &
 varname79, arrayname79, &
 varname80, arrayname80, &
 varname81, arrayname81, &
 varname82, arrayname82, &
 varname83, arrayname83, &
 varname84, arrayname84, &
 varname85, arrayname85, &
 varname86, arrayname86, &
 varname87, arrayname87, &
 varname88, arrayname88, &
 varname89, arrayname89, &
 varname90, arrayname90, &
 varname91, arrayname91, &
 varname92, arrayname92, &
 varname93, arrayname93, &
 varname94, arrayname94, &
 varname95, arrayname95  &
                                                )
 
 END SUBROUTINE JSON_ARGS 
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine check_json_for_stuff_doesnt_belong (json,nmlname,& 
 varname1, arrayname1, a2dname1, &
 varname2, arrayname2, a2dname2, &
 varname3, arrayname3, a2dname3, &
 varname4, arrayname4, a2dname4, &
 varname5, arrayname5, a2dname5, &
 varname6, arrayname6, a2dname6, &
 varname7, arrayname7, a2dname7, &
 varname8, arrayname8, a2dname8, &
 varname9, arrayname9, a2dname9, &
 varname10, arrayname10, a2dname10, &
 varname11, arrayname11, a2dname11, &
 varname12, arrayname12, a2dname12, &
 varname13, arrayname13, a2dname13, &
 varname14, arrayname14, a2dname14, &
 varname15, arrayname15, a2dname15, &
 varname16, arrayname16, a2dname16, &
 varname17, arrayname17, a2dname17, &
 varname18, arrayname18, a2dname18, &
 varname19, arrayname19, a2dname19, &
 varname20, arrayname20, a2dname20, &
 varname21, arrayname21, &
 varname22, arrayname22, &
 varname23, arrayname23, &
 varname24, arrayname24, &
 varname25, arrayname25, &
 varname26, arrayname26, &
 varname27, arrayname27, &
 varname28, arrayname28, &
 varname29, arrayname29, &
 varname30, arrayname30, &
 varname31, arrayname31, &
 varname32, arrayname32, &
 varname33, arrayname33, &
 varname34, arrayname34, &
 varname35, arrayname35, &
 varname36, arrayname36, &
 varname37, arrayname37, &
 varname38, arrayname38, &
 varname39, arrayname39, &
 varname40, arrayname40, &
 varname41, arrayname41, &
 varname42, arrayname42, &
 varname43, arrayname43, &
 varname44, arrayname44, &
 varname45, arrayname45, &
 varname46, arrayname46, &
 varname47, arrayname47, &
 varname48, arrayname48, &
 varname49, arrayname49, &
 varname50, arrayname50, &
 varname51, arrayname51, &
 varname52, arrayname52, &
 varname53, arrayname53, &
 varname54, arrayname54, &
 varname55, arrayname55, &
 varname56, arrayname56, &
 varname57, arrayname57, &
 varname58, arrayname58, &
 varname59, arrayname59, &
 varname60, arrayname60, &
 varname61, arrayname61, &
 varname62, arrayname62, &
 varname63, arrayname63, &
 varname64, arrayname64, &
 varname65, arrayname65, &
 varname66, arrayname66, &
 varname67, arrayname67, &
 varname68, arrayname68, &
 varname69, arrayname69, &
 varname70, arrayname70, &
 varname71, arrayname71, &
 varname72, arrayname72, &
 varname73, arrayname73, &
 varname74, arrayname74, &
 varname75, arrayname75, &
 varname76, arrayname76, &
 varname77, arrayname77, &
 varname78, arrayname78, &
 varname79, arrayname79, &
 varname80, arrayname80, &
 varname81, arrayname81, &
 varname82, arrayname82, &
 varname83, arrayname83, &
 varname84, arrayname84, &
 varname85, arrayname85, &
 varname86, arrayname86, &
 varname87, arrayname87, &
 varname88, arrayname88, &
 varname89, arrayname89, &
 varname90, arrayname90, &
 varname91, arrayname91, &
 varname92, arrayname92, &
 varname93, arrayname93, &
 varname94, arrayname94, &
 varname95, arrayname95  &
                                                )
  character (*),intent(in) :: nmlname
  character (*),intent(in) :: json
 character (*),intent(in),optional ::  varname1, arrayname1
 character (*),intent(in),optional ::  varname2, arrayname2
 character (*),intent(in),optional ::  varname3, arrayname3
 character (*),intent(in),optional ::  varname4, arrayname4
 character (*),intent(in),optional ::  varname5, arrayname5
 character (*),intent(in),optional ::  varname6, arrayname6
 character (*),intent(in),optional ::  varname7, arrayname7
 character (*),intent(in),optional ::  varname8, arrayname8
 character (*),intent(in),optional ::  varname9, arrayname9
 character (*),intent(in),optional ::  varname10, arrayname10
 character (*),intent(in),optional ::  varname11, arrayname11
 character (*),intent(in),optional ::  varname12, arrayname12
 character (*),intent(in),optional ::  varname13, arrayname13
 character (*),intent(in),optional ::  varname14, arrayname14
 character (*),intent(in),optional ::  varname15, arrayname15
 character (*),intent(in),optional ::  varname16, arrayname16
 character (*),intent(in),optional ::  varname17, arrayname17
 character (*),intent(in),optional ::  varname18, arrayname18
 character (*),intent(in),optional ::  varname19, arrayname19
 character (*),intent(in),optional ::  varname20, arrayname20
 character (*),intent(in),optional ::  varname21, arrayname21
 character (*),intent(in),optional ::  varname22, arrayname22
 character (*),intent(in),optional ::  varname23, arrayname23
 character (*),intent(in),optional ::  varname24, arrayname24
 character (*),intent(in),optional ::  varname25, arrayname25
 character (*),intent(in),optional ::  varname26, arrayname26
 character (*),intent(in),optional ::  varname27, arrayname27
 character (*),intent(in),optional ::  varname28, arrayname28
 character (*),intent(in),optional ::  varname29, arrayname29
 character (*),intent(in),optional ::  varname30, arrayname30
 character (*),intent(in),optional ::  varname31, arrayname31
 character (*),intent(in),optional ::  varname32, arrayname32
 character (*),intent(in),optional ::  varname33, arrayname33
 character (*),intent(in),optional ::  varname34, arrayname34
 character (*),intent(in),optional ::  varname35, arrayname35
 character (*),intent(in),optional ::  varname36, arrayname36
 character (*),intent(in),optional ::  varname37, arrayname37
 character (*),intent(in),optional ::  varname38, arrayname38
 character (*),intent(in),optional ::  varname39, arrayname39
 character (*),intent(in),optional ::  varname40, arrayname40
 character (*),intent(in),optional ::  varname41, arrayname41
 character (*),intent(in),optional ::  varname42, arrayname42
 character (*),intent(in),optional ::  varname43, arrayname43
 character (*),intent(in),optional ::  varname44, arrayname44
 character (*),intent(in),optional ::  varname45, arrayname45
 character (*),intent(in),optional ::  varname46, arrayname46
 character (*),intent(in),optional ::  varname47, arrayname47
 character (*),intent(in),optional ::  varname48, arrayname48
 character (*),intent(in),optional ::  varname49, arrayname49
 character (*),intent(in),optional ::  varname50, arrayname50
 character (*),intent(in),optional ::  varname51, arrayname51
 character (*),intent(in),optional ::  varname52, arrayname52
 character (*),intent(in),optional ::  varname53, arrayname53
 character (*),intent(in),optional ::  varname54, arrayname54
 character (*),intent(in),optional ::  varname55, arrayname55
 character (*),intent(in),optional ::  varname56, arrayname56
 character (*),intent(in),optional ::  varname57, arrayname57
 character (*),intent(in),optional ::  varname58, arrayname58
 character (*),intent(in),optional ::  varname59, arrayname59
 character (*),intent(in),optional ::  varname60, arrayname60
 character (*),intent(in),optional ::  varname61, arrayname61
 character (*),intent(in),optional ::  varname62, arrayname62
 character (*),intent(in),optional ::  varname63, arrayname63
 character (*),intent(in),optional ::  varname64, arrayname64
 character (*),intent(in),optional ::  varname65, arrayname65
 character (*),intent(in),optional ::  varname66, arrayname66
 character (*),intent(in),optional ::  varname67, arrayname67
 character (*),intent(in),optional ::  varname68, arrayname68
 character (*),intent(in),optional ::  varname69, arrayname69
 character (*),intent(in),optional ::  varname70, arrayname70
 character (*),intent(in),optional ::  varname71, arrayname71
 character (*),intent(in),optional ::  varname72, arrayname72
 character (*),intent(in),optional ::  varname73, arrayname73
 character (*),intent(in),optional ::  varname74, arrayname74
 character (*),intent(in),optional ::  varname75, arrayname75
 character (*),intent(in),optional ::  varname76, arrayname76
 character (*),intent(in),optional ::  varname77, arrayname77
 character (*),intent(in),optional ::  varname78, arrayname78
 character (*),intent(in),optional ::  varname79, arrayname79
 character (*),intent(in),optional ::  varname80, arrayname80
 character (*),intent(in),optional ::  varname81, arrayname81
 character (*),intent(in),optional ::  varname82, arrayname82
 character (*),intent(in),optional ::  varname83, arrayname83
 character (*),intent(in),optional ::  varname84, arrayname84
 character (*),intent(in),optional ::  varname85, arrayname85
 character (*),intent(in),optional ::  varname86, arrayname86
 character (*),intent(in),optional ::  varname87, arrayname87
 character (*),intent(in),optional ::  varname88, arrayname88
 character (*),intent(in),optional ::  varname89, arrayname89
 character (*),intent(in),optional ::  varname90, arrayname90
 character (*),intent(in),optional ::  varname91, arrayname91
 character (*),intent(in),optional ::  varname92, arrayname92
 character (*),intent(in),optional ::  varname93, arrayname93
 character (*),intent(in),optional ::  varname94, arrayname94
 character (*),intent(in),optional ::  varname95, arrayname95
 character (*),intent(in),optional ::  a2dname1
 character (*),intent(in),optional ::  a2dname2
 character (*),intent(in),optional ::  a2dname3
 character (*),intent(in),optional ::  a2dname4
 character (*),intent(in),optional ::  a2dname5
 character (*),intent(in),optional ::  a2dname6
 character (*),intent(in),optional ::  a2dname7
 character (*),intent(in),optional ::  a2dname8
 character (*),intent(in),optional ::  a2dname9
 character (*),intent(in),optional ::  a2dname10
 character (*),intent(in),optional ::  a2dname11
 character (*),intent(in),optional ::  a2dname12
 character (*),intent(in),optional ::  a2dname13
 character (*),intent(in),optional ::  a2dname14
 character (*),intent(in),optional ::  a2dname15
 character (*),intent(in),optional ::  a2dname16
 character (*),intent(in),optional ::  a2dname17
 character (*),intent(in),optional ::  a2dname18
 character (*),intent(in),optional ::  a2dname19
 character (*),intent(in),optional ::  a2dname20
 integer :: i,j,k,l
 logical :: level1=.false. , level2=.false. , quote = .false. , paren = .false.
  character (len=:),allocatable :: thevariable
 LOGICAL :: ISITINTHENAMELIST = .false.
 integer :: nmlflag
 
 nmlflag = namelist_exist(json,trim(nmlname))
 IF (NMLFLAG > 0) THEN !> Check if the nml exists.  If it doesnt, then process it.
 findnml: do i=1,len(json)
 !   write (6,*)trim(nmlname),json(i:i+len(nmlname) ),len(nmlname)
    if (trim(nmlname) == json(i:i+len(nmlname)-1) ) then 
      do j=i+len(nmlname)-1,len(json)
           if(json(j:j) == "[" ) then
            l=j
            exit findnml
           endif
      enddo
    endif
  enddo findnml
 
 
 findvars: do i=l+1,len(json)
 if (json(i:i) == "]" .and. .not.level1 .and. .not.level2)then
                 exit findvars
 elseif (json(i:i) == "[" .and. .not.level1) then
            level1=.true.
 elseif (json(i:i) == "[" .and. level1 .and. .not.level2)then
             level2=.true.
 elseif (json(i:i) == "]" .and. level1 .and. .not.level2)then
             level1 = .false.
 elseif (json(i:i) == "]" .and. level1 .and. level2)then
             level2=.false.
 elseif (json(i:i)==":".and. .not.level1 .and. .not.level2 .AND.& 
                   .not.paren  .AND. .not.quote ) then
              do j=i-1,l,-1
                   if (json(j:j) == '"' .and. .not.quote) then
                                 quote = .true.
                                 k=j-1
                   elseif (json(j:j) == '"' .and. quote) then
                                 quote = .false. 
                                 exit
                   elseif (json(j:j) == ")" .and. .not.paren) then
                                 paren = .true.
                   elseif (json(j:j) == "(" .and. paren) then
                                 paren = .false. 
                                 k=j-1
                   elseif (json(j:j) == "(" .and. .not.paren) then
                                 exit
                   elseif ( .not.paren .AND. quote  )then
                                 thevariable = json(j:k)
                   endif
              enddo
                paren=.false. ; quote=.false.
     if (thevariable == "description") ISITINTHENAMELIST = .true. ! comment
     if (thevariable == "") ISITINTHENAMELIST = .true.            ! comment
     if( present(varname1) .AND. strcase(thevariable) == strcase(varname1)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname1) .AND. strcase(thevariable) == strcase(arrayname1)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname1) .AND. strcase(thevariable) == strcase(a2dname1)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname2) .AND. strcase(thevariable) == strcase(varname2)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname2) .AND. strcase(thevariable) == strcase(arrayname2)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname2) .AND. strcase(thevariable) == strcase(a2dname2)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname3) .AND. strcase(thevariable) == strcase(varname3)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname3) .AND. strcase(thevariable) == strcase(arrayname3)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname3) .AND. strcase(thevariable) == strcase(a2dname3)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname4) .AND. strcase(thevariable) == strcase(varname4)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname4) .AND. strcase(thevariable) == strcase(arrayname4)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname4) .AND. strcase(thevariable) == strcase(a2dname4)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname5) .AND. strcase(thevariable) == strcase(varname5)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname5) .AND. strcase(thevariable) == strcase(arrayname5)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname5) .AND. strcase(thevariable) == strcase(a2dname5)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname6) .AND. strcase(thevariable) == strcase(varname6)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname6) .AND. strcase(thevariable) == strcase(arrayname6)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname6) .AND. strcase(thevariable) == strcase(a2dname6)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname7) .AND. strcase(thevariable) == strcase(varname7)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname7) .AND. strcase(thevariable) == strcase(arrayname7)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname7) .AND. strcase(thevariable) == strcase(a2dname7)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname8) .AND. strcase(thevariable) == strcase(varname8)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname8) .AND. strcase(thevariable) == strcase(arrayname8)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname8) .AND. strcase(thevariable) == strcase(a2dname8)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname9) .AND. strcase(thevariable) == strcase(varname9)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname9) .AND. strcase(thevariable) == strcase(arrayname9)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname9) .AND. strcase(thevariable) == strcase(a2dname9)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname10) .AND. strcase(thevariable) == strcase(varname10)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname10) .AND. strcase(thevariable) == strcase(arrayname10)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname10) .AND. strcase(thevariable) == strcase(a2dname10)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname11) .AND. strcase(thevariable) == strcase(varname11)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname11) .AND. strcase(thevariable) == strcase(arrayname11)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname11) .AND. strcase(thevariable) == strcase(a2dname11)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname12) .AND. strcase(thevariable) == strcase(varname12)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname12) .AND. strcase(thevariable) == strcase(arrayname12)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname12) .AND. strcase(thevariable) == strcase(a2dname12)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname13) .AND. strcase(thevariable) == strcase(varname13)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname13) .AND. strcase(thevariable) == strcase(arrayname13)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname13) .AND. strcase(thevariable) == strcase(a2dname13)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname14) .AND. strcase(thevariable) == strcase(varname14)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname14) .AND. strcase(thevariable) == strcase(arrayname14)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname14) .AND. strcase(thevariable) == strcase(a2dname14)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname15) .AND. strcase(thevariable) == strcase(varname15)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname15) .AND. strcase(thevariable) == strcase(arrayname15)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname15) .AND. strcase(thevariable) == strcase(a2dname15)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname16) .AND. strcase(thevariable) == strcase(varname16)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname16) .AND. strcase(thevariable) == strcase(arrayname16)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname16) .AND. strcase(thevariable) == strcase(a2dname16)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname17) .AND. strcase(thevariable) == strcase(varname17)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname17) .AND. strcase(thevariable) == strcase(arrayname17)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname17) .AND. strcase(thevariable) == strcase(a2dname17)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname18) .AND. strcase(thevariable) == strcase(varname18)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname18) .AND. strcase(thevariable) == strcase(arrayname18)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname18) .AND. strcase(thevariable) == strcase(a2dname18)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname19) .AND. strcase(thevariable) == strcase(varname19)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname19) .AND. strcase(thevariable) == strcase(arrayname19)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname19) .AND. strcase(thevariable) == strcase(a2dname19)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname20) .AND. strcase(thevariable) == strcase(varname20)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname20) .AND. strcase(thevariable) == strcase(arrayname20)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(a2dname20) .AND. strcase(thevariable) == strcase(a2dname20)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname21) .AND. strcase(thevariable) == strcase(varname21)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname21) .AND. strcase(thevariable) == strcase(arrayname21)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname22) .AND. strcase(thevariable) == strcase(varname22)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname22) .AND. strcase(thevariable) == strcase(arrayname22)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname23) .AND. strcase(thevariable) == strcase(varname23)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname23) .AND. strcase(thevariable) == strcase(arrayname23)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname24) .AND. strcase(thevariable) == strcase(varname24)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname24) .AND. strcase(thevariable) == strcase(arrayname24)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname25) .AND. strcase(thevariable) == strcase(varname25)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname25) .AND. strcase(thevariable) == strcase(arrayname25)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname26) .AND. strcase(thevariable) == strcase(varname26)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname26) .AND. strcase(thevariable) == strcase(arrayname26)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname27) .AND. strcase(thevariable) == strcase(varname27)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname27) .AND. strcase(thevariable) == strcase(arrayname27)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname28) .AND. strcase(thevariable) == strcase(varname28)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname28) .AND. strcase(thevariable) == strcase(arrayname28)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname29) .AND. strcase(thevariable) == strcase(varname29)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname29) .AND. strcase(thevariable) == strcase(arrayname29)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname30) .AND. strcase(thevariable) == strcase(varname30)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname30) .AND. strcase(thevariable) == strcase(arrayname30)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname31) .AND. strcase(thevariable) == strcase(varname31)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname31) .AND. strcase(thevariable) == strcase(arrayname31)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname32) .AND. strcase(thevariable) == strcase(varname32)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname32) .AND. strcase(thevariable) == strcase(arrayname32)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname33) .AND. strcase(thevariable) == strcase(varname33)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname33) .AND. strcase(thevariable) == strcase(arrayname33)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname34) .AND. strcase(thevariable) == strcase(varname34)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname34) .AND. strcase(thevariable) == strcase(arrayname34)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname35) .AND. strcase(thevariable) == strcase(varname35)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname35) .AND. strcase(thevariable) == strcase(arrayname35)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname36) .AND. strcase(thevariable) == strcase(varname36)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname36) .AND. strcase(thevariable) == strcase(arrayname36)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname37) .AND. strcase(thevariable) == strcase(varname37)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname37) .AND. strcase(thevariable) == strcase(arrayname37)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname38) .AND. strcase(thevariable) == strcase(varname38)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname38) .AND. strcase(thevariable) == strcase(arrayname38)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname39) .AND. strcase(thevariable) == strcase(varname39)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname39) .AND. strcase(thevariable) == strcase(arrayname39)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname40) .AND. strcase(thevariable) == strcase(varname40)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname40) .AND. strcase(thevariable) == strcase(arrayname40)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname41) .AND. strcase(thevariable) == strcase(varname41)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname41) .AND. strcase(thevariable) == strcase(arrayname41)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname42) .AND. strcase(thevariable) == strcase(varname42)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname42) .AND. strcase(thevariable) == strcase(arrayname42)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname43) .AND. strcase(thevariable) == strcase(varname43)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname43) .AND. strcase(thevariable) == strcase(arrayname43)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname44) .AND. strcase(thevariable) == strcase(varname44)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname44) .AND. strcase(thevariable) == strcase(arrayname44)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname45) .AND. strcase(thevariable) == strcase(varname45)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname45) .AND. strcase(thevariable) == strcase(arrayname45)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname46) .AND. strcase(thevariable) == strcase(varname46)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname46) .AND. strcase(thevariable) == strcase(arrayname46)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname47) .AND. strcase(thevariable) == strcase(varname47)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname47) .AND. strcase(thevariable) == strcase(arrayname47)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname48) .AND. strcase(thevariable) == strcase(varname48)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname48) .AND. strcase(thevariable) == strcase(arrayname48)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname49) .AND. strcase(thevariable) == strcase(varname49)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname49) .AND. strcase(thevariable) == strcase(arrayname49)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname50) .AND. strcase(thevariable) == strcase(varname50)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname50) .AND. strcase(thevariable) == strcase(arrayname50)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname51) .AND. strcase(thevariable) == strcase(varname51)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname51) .AND. strcase(thevariable) == strcase(arrayname51)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname52) .AND. strcase(thevariable) == strcase(varname52)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname52) .AND. strcase(thevariable) == strcase(arrayname52)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname53) .AND. strcase(thevariable) == strcase(varname53)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname53) .AND. strcase(thevariable) == strcase(arrayname53)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname54) .AND. strcase(thevariable) == strcase(varname54)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname54) .AND. strcase(thevariable) == strcase(arrayname54)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname55) .AND. strcase(thevariable) == strcase(varname55)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname55) .AND. strcase(thevariable) == strcase(arrayname55)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname56) .AND. strcase(thevariable) == strcase(varname56)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname56) .AND. strcase(thevariable) == strcase(arrayname56)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname57) .AND. strcase(thevariable) == strcase(varname57)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname57) .AND. strcase(thevariable) == strcase(arrayname57)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname58) .AND. strcase(thevariable) == strcase(varname58)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname58) .AND. strcase(thevariable) == strcase(arrayname58)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname59) .AND. strcase(thevariable) == strcase(varname59)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname59) .AND. strcase(thevariable) == strcase(arrayname59)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname60) .AND. strcase(thevariable) == strcase(varname60)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname60) .AND. strcase(thevariable) == strcase(arrayname60)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname61) .AND. strcase(thevariable) == strcase(varname61)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname61) .AND. strcase(thevariable) == strcase(arrayname61)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname62) .AND. strcase(thevariable) == strcase(varname62)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname62) .AND. strcase(thevariable) == strcase(arrayname62)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname63) .AND. strcase(thevariable) == strcase(varname63)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname63) .AND. strcase(thevariable) == strcase(arrayname63)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname64) .AND. strcase(thevariable) == strcase(varname64)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname64) .AND. strcase(thevariable) == strcase(arrayname64)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname65) .AND. strcase(thevariable) == strcase(varname65)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname65) .AND. strcase(thevariable) == strcase(arrayname65)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname66) .AND. strcase(thevariable) == strcase(varname66)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname66) .AND. strcase(thevariable) == strcase(arrayname66)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname67) .AND. strcase(thevariable) == strcase(varname67)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname67) .AND. strcase(thevariable) == strcase(arrayname67)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname68) .AND. strcase(thevariable) == strcase(varname68)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname68) .AND. strcase(thevariable) == strcase(arrayname68)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname69) .AND. strcase(thevariable) == strcase(varname69)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname69) .AND. strcase(thevariable) == strcase(arrayname69)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname70) .AND. strcase(thevariable) == strcase(varname70)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname70) .AND. strcase(thevariable) == strcase(arrayname70)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname71) .AND. strcase(thevariable) == strcase(varname71)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname71) .AND. strcase(thevariable) == strcase(arrayname71)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname72) .AND. strcase(thevariable) == strcase(varname72)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname72) .AND. strcase(thevariable) == strcase(arrayname72)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname73) .AND. strcase(thevariable) == strcase(varname73)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname73) .AND. strcase(thevariable) == strcase(arrayname73)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname74) .AND. strcase(thevariable) == strcase(varname74)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname74) .AND. strcase(thevariable) == strcase(arrayname74)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname75) .AND. strcase(thevariable) == strcase(varname75)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname75) .AND. strcase(thevariable) == strcase(arrayname75)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname76) .AND. strcase(thevariable) == strcase(varname76)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname76) .AND. strcase(thevariable) == strcase(arrayname76)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname77) .AND. strcase(thevariable) == strcase(varname77)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname77) .AND. strcase(thevariable) == strcase(arrayname77)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname78) .AND. strcase(thevariable) == strcase(varname78)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname78) .AND. strcase(thevariable) == strcase(arrayname78)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname79) .AND. strcase(thevariable) == strcase(varname79)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname79) .AND. strcase(thevariable) == strcase(arrayname79)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname80) .AND. strcase(thevariable) == strcase(varname80)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname80) .AND. strcase(thevariable) == strcase(arrayname80)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname81) .AND. strcase(thevariable) == strcase(varname81)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname81) .AND. strcase(thevariable) == strcase(arrayname81)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname82) .AND. strcase(thevariable) == strcase(varname82)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname82) .AND. strcase(thevariable) == strcase(arrayname82)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname83) .AND. strcase(thevariable) == strcase(varname83)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname83) .AND. strcase(thevariable) == strcase(arrayname83)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname84) .AND. strcase(thevariable) == strcase(varname84)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname84) .AND. strcase(thevariable) == strcase(arrayname84)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname85) .AND. strcase(thevariable) == strcase(varname85)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname85) .AND. strcase(thevariable) == strcase(arrayname85)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname86) .AND. strcase(thevariable) == strcase(varname86)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname86) .AND. strcase(thevariable) == strcase(arrayname86)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname87) .AND. strcase(thevariable) == strcase(varname87)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname87) .AND. strcase(thevariable) == strcase(arrayname87)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname88) .AND. strcase(thevariable) == strcase(varname88)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname88) .AND. strcase(thevariable) == strcase(arrayname88)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname89) .AND. strcase(thevariable) == strcase(varname89)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname89) .AND. strcase(thevariable) == strcase(arrayname89)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname90) .AND. strcase(thevariable) == strcase(varname90)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname90) .AND. strcase(thevariable) == strcase(arrayname90)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname91) .AND. strcase(thevariable) == strcase(varname91)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname91) .AND. strcase(thevariable) == strcase(arrayname91)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname92) .AND. strcase(thevariable) == strcase(varname92)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname92) .AND. strcase(thevariable) == strcase(arrayname92)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname93) .AND. strcase(thevariable) == strcase(varname93)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname93) .AND. strcase(thevariable) == strcase(arrayname93)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname94) .AND. strcase(thevariable) == strcase(varname94)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname94) .AND. strcase(thevariable) == strcase(arrayname94)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(varname95) .AND. strcase(thevariable) == strcase(varname95)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if( present(arrayname95) .AND. strcase(thevariable) == strcase(arrayname95)) then 
         ISITINTHENAMELIST = .true.
     endif 
     if (ISITINTHENAMELIST .eqv. .FALSE.) then
        call cjson_error_mesg("cjson_wrapper_mod:check_json_for_stuff_doesnt_belong", &
          thevariable//" is not listed as a member of "//nmlname//" in JSON_ARGS",  & 
          FATAL)
     endif
             ISITINTHENAMELIST = .false.
         endif
 
 enddo findvars
 ENDIF
 
 end subroutine check_json_for_stuff_doesnt_belong
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 SUBROUTINE missing_name(nmlname)
  character (*)                          :: nmlname 
        call cjson_error_mesg("cjson_wrapper_mod:missing_name",         &
          "Name of an array variable in "//trim(nmlname)//" is missing",&
           FATAL)
 END SUBROUTINE missing_name
  end module cjson_wrapper_mod
