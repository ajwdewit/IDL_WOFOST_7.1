; IDL version of the WOFOST crop simulation model version 7.1
;
; Copyright 2016 Institute of Remote Sensing and Digital Earth, Chinese Academy of Sciences
; Authors: Chen zhiqiang (chengzq@radi.ac.cn), Meng Jihua (mengjh@radi.ac.cn) and 
;          Wang Yiming
; Reference: http://www.mdpi.com/2072-4292/8/4/303
; Release date: 2016-08-24
; Encoding: UTF-8
;
; This file is part of the IDL-WOFOST implementation version 7.1
;
; IDL-WOFOST is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;
;---------------------------------------------------------------------------------------------------------------
;Writted by Cheng Zhiqiang
;Finish time is 2014-11-19
;Modify time is 2014-12-01
;Asiimilation method build time is 2015-5-1
;The model optimization time 2016-8-25
;Please choose the right path for the input data
;Please create three empty files for NDVI,biomass and yield
;Please choose the word GB18030
;Please build the project again if the system can't work normally or you modify any code
;The code is translated forme the file named W60LIB and  W70MAIN.FOR, we selected the necessary code to simualte the crop growth and realise the RS assimulation
;Some input, output, or data process code is not included in this project, such as the module APTOCTB.FOR,CROPNO.FOR,PAUZE.FOR,PRHEAD.FOR,PRIWGW.FOR,PRIWPP.FOR
;We have change some algorithms of soil nutrients to meet the demand in our study, including soil nutrient absorb, fertilization, soil nutrient stress, they can be regained in yield_nutrie.pro
;A special assimilation method is showed in this project(http://www.mdpi.com/2072-4292/8/4/303), New code is necessary when applied into another study or assimilation method.
;The modules of FSEOPT,TTUTIL,CABOWE which may be necessary in other studies and they can be added into this project by built new fuctions or pros
;Anyother questions plaease contacts with author, E-mail chengzq@radi.ac.cn, mengjh@radi.ac.cn
;---------------------------------------------------------------------------------------------------------------

PRO WOFOST
  COMPILE_OPT idl2
  ;主程序，对整个生长过程进行模拟，对其它程序模块进行调用（The main project, simulate the crop growth and calculate the yield with the help of other project, 
  ;corresponds to the CLIMRD.FOR, CONVR2.FOR,CROPSI.FOR,DATES.FOR,GAMMA2.FOR,METEO.FOR,PRIJRC.FOR,PRIWFD.FOR,RANDOM.FOR,RDMINF.FOR,REPRD.FOR,STATPP.FOR
  ;STATWP.FOR,WOFSIM.FOR,WOFOST7.FOR，W70MAIN.FOR modules of orginal WOFOST in fortran ）
  ;-------------------------------------------------------------------------------------------------------------
  ;作物参数输入区 (parameter input)
  crop_cut = 1                                           ;Whether the crop distribution map is needed,0 for no,1 for yes
  crop_type = 1                                          ;Crop type to be simulated in the model,1for maize,2 for soybean,3 for wheat,0 for others
  parameter_txt_path = 'D:\1\1\WOFOST20160825\Document\'                  ; The parameter file path, crop and soil parameter
;  parameter_input,crop_type,parameter_txt_path,DELT,IWB,FLTB1,FLTB2,TEFFMX,CRAIRC,I_nutrient,I_stess,IOX,IAIRDU,i_stday,stday_start,stday_end,DEPNR,TSUM1,TSUM2,SMFCF,SMW,DVS,SM0,RMR,RMO,RML,RMS,Q10,CVL,CVO,CVR,CVS,TDWI,Ju1,DVSEND,SPAN,PERDL,TBASE,RDI,RRI,SPA,RDMCR,RDMSOL,RGRLAI,ISTATE,LASUM,RAINT,IDEM,K0,LAI_start,TSUMEM,NOTINF,SM_t,SMSLATB1,TBASEM,CFET,SPADS,SPODS,SPASS,SPOSS,SLATB1,SLATB2,SSATB1,SSATB2,FRTB1,FRTB2,FSTB1,FSTB2,FOTB1,FOTB2,DTSMTB1,DTSMTB2,AMAXTB1,AMAXTB2,TMPFTB1,TMPFTB2,EEFTB1,EEFTB2,KDIFTB1,KDIFTB2,RFSETB1,RFSETB2,RDRSTB1,RDRSTB2,RDRRTB1,RDRRTB2,SSI,XDEF,ZTI,IDRAIN,DD,SMTAB1,SMTAB2,CONTAB1,CONTAB2,SOPE,KSUB
 ;-------------------------------------------------------------------------------------
  ;Data input

  HJ_file = 'D:\biomass\result_wofost\NDVI\2014\'                         ; Remote sensing input path                         
  lat$ = 'D:\biomass\result_wofost\LAT_DEM\new\1\hongxing_LAT.tif'        ; latitude input path        
  DEM$ = 'D:\biomass\result_wofost\LAT_DEM\new\1\hongxing_DEM.tif'        ; DEM input path           
  meto_text = 'D:\biomass\result_wofost\wofost_text\2014.txt'             ; meteorological data input path 
  out_simulation = 'D:\biomass\result_wofost\out\'                        ; output path 
;  out_bio = 'E:\biomass\result_wofost\bio\2014\soil\soybean\'                  
;  out_LAI = 'E:\biomass\result_wofost\lai\2014\soil\soybean\'                               
;  out_yield = 'E:\biomass\result_wofost\yield\2014\soil\soybean\'                       
  NDVI_file = 'D:\biomass\result_wofost\NDVI\2014\1\'                                   ; NDVI input-output path        
  crop_distribution$ = 'D:\biomass\result_wofost\crop_type\hx_2014_wofost.tif'          ; croptype file input path   
 
  FILE_MKDIR,out_simulation  + 'lai\'
  FILE_MKDIR,out_simulation  + 'yield\'
  FILE_MKDIR,out_simulation  + 'bio\'
  
  out_bio = out_simulation  + 'bio\'
  out_lai = out_simulation  + 'lai\'
  out_yield = out_simulation  + 'yield\'
  
  FILE_MKDIR,out_bio  + 'temporary\'
  FILE_MKDIR,out_lai  + 'temporary\'
  FILE_MKDIR,out_yield  + 'temporary\'
  
;  FILE_MKDIR,out_bio +'bio_total\'
;  FILE_MKDIR,out_bio +'waterlimit_yield\'
  FILE_MKDIR,HJ_file + '1\'
;  out_bio_total = out_bio +'bio_total\'
;  waterlimit_yield = out_bio +'waterlimit_yield\'
  NDVI_file = HJ_file + '1\'
  

  ; --------------------------------------------------------------------------------------------------------------
  ;文件输入输出路径选择
  ;      filters = ['*.tif;*.tiff','*.txt']
  ;      HJ_file = DIALOG_PICKFILE(PATH = 'D:\biomass\result_wofost\NDVI\2013\',/DIRECTORY,title = 'Remote sensing input path ' )                          ;环境星数据
  ;      lat$ = DIALOG_PICKFILE(PATH = 'D:\biomass\result_wofost\LAT_DEM\new\1\',/READ, FILTER = filters,title = 'latitude input path' )                   ;经纬度数据
  ;      DEM$ = DIALOG_PICKFILE(PATH = 'D:\biomass\result_wofost\LAT_DEM\new\1\',/READ, FILTER = filters,title = 'DEM input path' )                        ;高程数据
  ;      meto_text = DIALOG_PICKFILE(PATH = 'D:\biomass\result_wofost\wofost_text\',/READ, FILTER = filters,title = 'meteorological data input path ' )    ;气象数据
  ;      out_bio = DIALOG_PICKFILE(PATH = 'D:\biomass\result_wofost\bio\2013\' ,/DIRECTORY,title = 'biomass output path' )                                 ;生物量输出路径
  ;      out_LAI = DIALOG_PICKFILE(PATH = 'D:\biomass\result_wofost\LAI\2013\',/DIRECTORY,title = 'LAI output path' )                                      ;叶面积指数输出路径
  ;      out_yield = DIALOG_PICKFILE(PATH ='D:\biomass\result_wofost\yield\'  ,/DIRECTORY,title = 'yield output path' )                                    ;作物产量输出路径
  ;      NDVI_file = DIALOG_PICKFILE(PATH = 'D:\biomass\result_wofost\NDVI\2013\' ,/DIRECTORY,title = ' NDVI input-output path ' )                         ;NDVI插值输出路径
  ;      crop_distribution$ = DIALOG_PICKFILE(PATH ='D:\biomass\result_wofost\crop_type\' ,/READ, FILTER = filters,title = 'croptype file input path ' )   ;作物分布图
  ;-------------------------------------------------------------------------------------------------------------
  ;NDVI序列生成（Make time-series NDVI）
  NDVI_make,HJ_file = HJ_file,NDVI_file = NDVI_file,lat$ = lat$,DEM$ = DEM$
   ;-------------------------------------------------------------------------------------------------------------
    PI= 3.1415926
  file_ndviserise = FILE_SEARCH(NDVI_file+'*_second.tif',count=count)
  
  latitude_bolck = READ_TIFF(lat$)
  ;------------------------------------------------------------------------------------------------------------
  ;;输出数据格式定义(Output data format)
  size_DN = SIZE(latitude_bolck)
  nl_total=size_DN[2]
  ns=size_DN[1]
  Q_block_data = 2           ; The number of simulation blocks(determined by the computer RAM, remote sensing and study area)
;  Q_block_data = 1*cIAIRDU
  bio_total_net = FLTARR(ns,nl_total)
  yield_total = FLTARR(ns,nl_total)
  yield_total_nu = FLTARR(ns,nl_total)
  lai_total = FLTARR(ns,nl_total)
  bio_total_total = FLTARR(ns,nl_total)
  
  nl_block_lenth = long(nl_total / Q_block_data)
  
  FOR Q_block = 1, Q_block_data DO BEGIN
    ;----------------------------------------------------------------------------------------------------------------------------------
    ;Simulate one by one block
parameter_input,crop_type,parameter_txt_path,DELT,IWB,FLTB1,FLTB2,TEFFMX,CRAIRC,I_nutrient,I_stess,IOX,IAIRDU,i_stday,stday_start,stday_end,DEPNR,TSUM1,TSUM2,SMFCF,SMW,DVS,SM0,RMR,RMO,RML,RMS,Q10,CVL,CVO,CVR,CVS,TDWI,Ju1,DVSEND,SPAN,PERDL,TBASE,RDI,RRI,SPA,RDMCR,RDMSOL,RGRLAI,ISTATE,LASUM,RAINT,IDEM,K0,LAI_start,TSUMEM,NOTINF,SM_t,SMSLATB1,TBASEM,CFET,SPADS,SPODS,SPASS,SPOSS,SLATB1,SLATB2,SSATB1,SSATB2,FRTB1,FRTB2,FSTB1,FSTB2,FOTB1,FOTB2,DTSMTB1,DTSMTB2,AMAXTB1,AMAXTB2,TMPFTB1,TMPFTB2,EEFTB1,EEFTB2,KDIFTB1,KDIFTB2,RFSETB1,RFSETB2,RDRSTB1,RDRSTB2,RDRRTB1,RDRRTB2
  ;-----------------------------------------------------------------------------------------------------------------------------------------
  ;参数传递数量超限，部分公共参数单独调用（The parameter passing number exceed the max allowable quantity,some common Parameters accepted in this place）
  ;部分公共参数（Some common Parameters）
  SSI = 0.0                                                                                    ;土壤表面积水状况( Water condition in soil surface)
  XDEF = 16000                                                                                 ;土壤含水层最大深度(The maximum depth of soil aquifer)
  ZTI = 999                                                                                    ;初始地下水层的深度(The initial depth of underground aquifer)
  IDRAIN = 0.0                                                                                 ;是否有地下排水设施(Whether underground drainage can be used)
  DD = 0.0                                                                                     ;地下排水设施深度(The depth of underground drainage)
  SMTAB1 =[-1.000,1.000,1.300,1.491,2.000,2.400,2.700,3.400,4.204,6.000]                       ;土壤水分含量(soil moisture content as function of pF )
  SMTAB2 =[0.570,0.533,0.524,0.515,0.486,0.451,0.420,0.350,0.300,0.270]   
  CONTAB1 = [0.000,1.000,1.300,1.491,1.700,2.000,2.400,2.700,3.000,3.400,3.700,4.000,4.204]    ;土壤导水率(soil hydraulic conductivity as function of pF )
  CONTAB2 = [1.033,-0.824,-1.155,-1.398,-1.523,-1.959,-2.495,-2.886,-3.276,-3.770,-4.131,-4.481,-4.745]  
  SOPE = 0.55                                                                                  ;最大根系透水率(The maximum permeable rate of root zone) 
  KSUB = 0.37                                                                                  ;最大底层透水率 (Maximum percolation rate subsoil)  
  Longtitude_input = 116.0                                                                     ;区域中心的经度(The langtitude of the study area)
    IF crop_type EQ 1 THEN parameter_txt = parameter_txt_path + 'MAIZE_hongxing.txt'
    IF crop_type EQ 2 THEN parameter_txt = parameter_txt_path + 'SOYBEAN_hongxing.txt'
    IF crop_type EQ 3 THEN parameter_txt = parameter_txt_path + 'WHEAT_hongxing.txt'
    IF crop_type EQ 4 THEN parameter_txt = parameter_txt_path + 'MAIZE_hongxing.txt'
    OPENR,lun,parameter_txt,/get_lun
    lines = FILE_LINES(parameter_txt)
    para_all_data = STRARR(1,Lines)
    READF,lun,para_all_data
    for i = 0 ,lines -1 do begin
            para = para_all_data[i]
      para1 = STRSPLIT(para,'=',/EXTRACT)
      mark1 = para1[0]
      mark2 = STRTRIM(mark1,2)
      
   if mark2 eq 'SSI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SSI = FLOAT(para_data)
      endif
        if mark2 eq 'XDEF' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        XDEF = FLOAT(para_data)
      endif      
        if mark2 eq 'ZTI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        ZTI = FLOAT(para_data)
      endif
        if mark2 eq 'IDRAIN' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        IDRAIN = FLOAT(para_data)
      endif
        if mark2 eq 'DD' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        DD = FLOAT(para_data)
      endif
        if mark2 eq 'SMTAB1' then begin
          
          para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        SMTAB1 = para_d6
        
      endif
      
          if mark2 eq 'SMTAB2' then begin
       para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        SMTAB2 = para_d6
      endif    
      if mark2 eq 'CONTAB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        CONTAB1 = para_d6
      endif
          if mark2 eq 'CONTAB2' then begin
     para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        CONTAB2 = para_d6
      endif
          if mark2 eq 'SOPE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SOPE = FLOAT(para_data)
      endif
          if mark2 eq 'KSUB' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KSUB = FLOAT(para_data)
      endif
      endfor
          FREE_LUN,lun
 ;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ 
  
;parameter_input,crop_type,parameter_txt_path,DELT,IWB,FLTB1,FLTB2,TEFFMX,CRAIRC,I_nutrient,I_stess,IOX,IAIRDU,i_stday,stday_start,stday_end,DEPNR,TSUM1,TSUM2,SMFCF,SMW,DVS,SM0,RMR,RMO,RML,RMS,Q10,CVL,CVO,CVR,CVS,TDWI,Ju1,DVSEND,SPAN,PERDL,TBASE,RDI,RRI,SPA,RDMCR,RDMSOL,RGRLAI,ISTATE,LASUM,RAINT,IDEM,K0,LAI_start,TSUMEM,NOTINF,SM_t,SMSLATB1,TBASEM,CFET,SPADS,SPODS,SPASS,SPOSS,SLATB1,SLATB2,SSATB1,SSATB2,FRTB1,FRTB2,FSTB1,FSTB2,FOTB1,FOTB2,DTSMTB1,DTSMTB2,AMAXTB1,AMAXTB2,TMPFTB1,TMPFTB2,EEFTB1,EEFTB2,KDIFTB1,KDIFTB2,RFSETB1,RFSETB2,RDRSTB1,RDRSTB2,RDRRTB1,RDRRTB2,SSI,XDEF,ZTI,IDRAIN,DD,SMTAB1,SMTAB2,CONTAB1,CONTAB2,SOPE,KSUB

    
;    IWB = 1*cIWB
;    IOX = 1*cIOX
;    I_nutrient = 1*cinutrient
;    I_stess = 1 * cIstess +1
;;    IAIRDU = 1*cIAIRDU
;    i_stday = 1.0*cistday
;    year = cJu1[0]
;    
;    jday_start = julday(cJu1[1],cJu1[2], year) - julday(12,31, year -1)
;    jday_start1 = julday(cstart[1],cJu1[2], year)- julday(12,31, year -1)
;    jday_end = julday(cend[1],cJu1[2], year)- julday(12,31, year -1)
  
;    Ju1 = 1*jday_start
;    stday_start = 1*jday_start1
;    stday_end = 1*jday_end
;    
    Q_block_name1 = string(Q_block)
    Q_block_name = STRTRIM(Q_block_name1,2)
    
    out_bio_temporary = out_bio  + 'temporary\'
    out_lai_temporary = out_lai  + 'temporary\'
    out_yield_temporary = out_yield  + 'temporary\'
    
    IF Q_block LT Q_block_data THEN BEGIN
      nl_begin = (Q_block-1)*nl_block_lenth
      nl_end = Q_block*nl_block_lenth-1
      nl = nl_block_lenth
    ENDIF ELSE BEGIN
      nl_begin = (Q_block-1)*nl_block_lenth
      nl_end = nl_total - 1
      nl = nl_end -nl_begin +1
    ENDELSE
    
    latitude_10 = READ_TIFF(lat$,SUB_RECT=[0,nl_begin,ns,nl])
    latitude1 = FLOAT(latitude_10) * !PI / 180
    Longtitude = Longtitude_input* !PI / 180
    DEM1 = READ_TIFF(DEM$,SUB_RECT=[0,nl_begin,ns,nl])
    NDVI_count = N_ELEMENTS(file_ndviserise) - 1
    IF crop_cut EQ 1.0 THEN BEGIN
      crop_cut_data = READ_TIFF(crop_distribution$,SUB_RECT=[0,nl_begin,ns,nl])
    ENDIF
   ;------------------------------------------------------------------------------------------------------------
   ;输出数据格式定义(Output data format)
    bio_data=FLTARR(ns,nl)
    yield_data=FLTARR(ns,nl)
    LV = FLTARR(150,ns,nl)
    LVAGE = FLTARR(150)
    SLA = FLTARR(150,ns,nl)
    die_wso=FLTARR(ns,nl)
    die_wst=FLTARR(ns,nl)
    die_wlv=FLTARR(ns,nl)
    die_wrt=FLTARR(ns,nl)
    die_twso=FLTARR(ns,nl)
    die_twst=FLTARR(ns,nl)
    die_twlv=FLTARR(ns,nl)
    die_twrt=FLTARR(ns,nl)
    TWRT_DATA1=FLTARR(ns,nl)
    TWST_DATA1=FLTARR(ns,nl)
    TWSO_DATA1=FLTARR(ns,nl)
    TWLV_DATA1=FLTARR(ns,nl)
    DALV = FLTARR(ns,nl)
    LAI_RS = FLTARR(ns,nl)
    LAI_RSWF = FLTARR(ns,nl)
    LASUM = LAI_RSWF*0.5 +LASUM
    ;  MAXMIN_DATA = FLTARR(2,ns,nl)
    ;  crop_cut_rs = FLTARR(ns,nl)
    ;   uu=DIALOG_MESSAGE('cc')
    DTSUM = 0
    DVR = 0
    ;-------------------------------------------------------------------------------------------------------------
    ;数据初始化(data initialization)
    LAI_RSMX = 0.0
    LAI_RSMN = 7.0
    LAI_RSMN1 = 0.01
    LAI_RSMX1 = 6.0
    TERMNL = 0
    DVSI = 0.0
    SMW = 0.099
    DVS = DVSI
    TDWI = 100
    TSUM   = 0.
    IDANTH = -99
    ;LV = FLTARR(100)
    LVAGE = FLTARR(150)
    LVAGE[0] = 0.
    ILVOLD   = 1
    IDOST = 0
    IDWST = 0
    IDOSJ = 0
    IDWSJ = 0
    IDOS  = 0
    IDWS  = 0
    DWRT = 0.
    DWLV = 0.
    DWST = 0.
    DWSO = 0.
    crop_cut1 = 99
    AVARD_data = latitude1 *0.0
    DTGA_data = latitude1 *0.0
    LV_initial = latitude1 *0.0
    AF3 = DVS
    AF1 = SLATB1
    AF2 = SLATB2
    SLA_data= CAL_AFGEN(AF1,AF2,AF3)
    SLA[0,*,*] = latitude1 *0.0 +SLA_data
    AF1 = FRTB1
    AF2 = FRTB2
    FR = CAL_AFGEN(AF1,AF2,AF3)
    AF1 = FLTB1
    AF2 = FLTB2
    FL = CAL_AFGEN(AF1,AF2,AF3)
    AF1 = FSTB1
    AF2 = FSTB2
    FS = CAL_AFGEN(AF1,AF2,AF3)
    AF1 = FOTB1
    AF2 = FOTB2
    FO = CAL_AFGEN(AF1,AF2,AF3)
    IF ISTATE EQ 3.0 THEN BEGIN
      WRT_data  = latitude1 *0.0 +FR*TDWI
      TADW_data = latitude1 *0.0 +(1.-FR)*TDWI
      GWSO_data = latitude1 *0.0
      GWRT_data = latitude1 *0.0
      GWST_data = latitude1 *0.0
      WST_data  = FS*TADW_data
      WSO_data  = FO*TADW_data
      WLV_data  = FL*TADW_data
      cc = WST_data *0.0
      cc = REFORM(SLA[0,*,*])
      LAIEM_data = WLV_DATA*cc
      LASUM_data  = LAIEM_data
      LAIEXP_data = LAIEM_data
      LAIMAX_data  = LAIEM_data
      SSATB1 = [0.0,2.0]
      SSATB2 = [0.0,0.0]
      AF1 = SSATB1
      AF2 = SSATB2
      SSA = CAL_AFGEN(AF1,AF2,AF3)
      LAI_data = LASUM_data+SSA*WST_data+SPA*WSO_data
      ISTATE = ISTATE +1
    ENDIF ELSE BEGIN
      GWSO_data = latitude1 *0.0
      TADW_data = latitude1 *0.0
      GWSO_data = latitude1 *0.0
      GWST_data = latitude1 *0.0
      GWRT_data = latitude1 *0.0
      WRT_data = latitude1 *0.0
      WST_data = latitude1 *0.0
      WSO_data = latitude1 *0.0
      WLV_data = latitude1 *0.0
      LAIEM_data = WLV_DATA*SLA[0,*,*]*0.0
      LASUM_data  = LAIEM_data*0.0
      LAIEXP_data = LAIEM_data*0.0
      LAIMAX_data  = LAIEM_data*0.0
    ENDELSE
    REST_data = latitude1 *0.0
    LV_data = latitude1 *0.0
    SLAT_data = latitude1 *0.0
    TAGP= WLV_DATA + WST_DATA +WSO_DATA
    GASST = 0.
    MREST = 0.
    TRAT  = 0.
    TMINRA = 0.
    TSUME  = 0.
    DTSUME = 0.
    INHVS1 = 1
    INHVS2 = 1
    INHVS3 = 1
    LAI_data = latitude1 * 0.0 + 0.04836
    a_WFLAIRS = 10.0
    CCC = 'A'
    ;---------------------------------------------------------------------------------------------------
    ;播种时间计算(calculate the sow and emerge time)
;    sow_time,meto_text,file_ndviserise,SPASS,SPOSS,SPADS,SPODS,i_stday,stday_start,stday_end,ES0,RAIN,j_stday,Ju,ANGSTA, ANGSTB,Ju1
    sow_time ,meto_text,file_ndviserise,SPASS,SPOSS,SPADS,SPODS,i_stday,stday_start,stday_end,ES0,RAIN,j_stday,Ju,ANGSTA, ANGSTB,Ju1,NDVI_count,DELT,nl_begin,ns,nl
    ;---------------------------------------------------------------------------------------------------
    ;进度条（progress bar）
    tlb = WIDGET_BASE(xsize = 0,ysize = 0)
    WIDGET_CONTROL,tlb,/real
    prsbar = IDLITWDPROGRESSBAR(group_leader = tlb,title = 'Progress of the model. '+'The module is '+Q_block_name,cancel = cancelin)
    OPENR,lun,meto_text,/get_lun
    lines = FILE_LINES(meto_text)
    data = DBLARR(9,Lines)
    READF,lun,data
    ;----------------------------------------------------------------------------------------------------
    ;以DELT为步长对整个生长期进行模拟（simulate the crop growth by the step of DELT）
    ;  FOR J_WFLAIRS = 1.0,4.0,1.0 DO BEGIN
    ;  a_WFLAIRS = J_WFLAIRS
    FOR i_NDVI3 = 0, NDVI_count,DELT DO BEGIN
      i_bar = i_NDVI3*100/NDVI_count
      IF WIDGET_INFO(prsbar,/valid) THEN BEGIN
        IDLITWDPROGRESSBAR_setvalue,prsbar,i_bar
      ENDIF ELSE BEGIN
        tmp = DIALOG_MESSAGE('You have canceled the simulation, the breaking day is '  + STRTRIM(STRING(i_NDVI3),2),/info)
        BREAK
      ENDELSE
      ndviserise = READ_TIFF(file_ndviserise[i_NDVI3] ,SUB_RECT=[0,nl_begin,ns,nl],geotiff = geotiff)
      NDVI_data1 = ndviserise
      basename = FILE_BASENAME(file_ndviserise[i_NDVI3])
      year = STRMID(basename,8,4)
      month = STRMID(basename,12,2)
      day = STRMID(basename,14,2)
      date = {year: year, month: month, day: day}
      mark = STRMID(basename,8,8)
      j_stday = julday(month,day, year) - julday(12,31, year-1)
      
      IF j_stday LT Ju1 THEN BEGIN
        CONTINUE
      ENDIF ELSE BEGIN
      
      
        FOR i_meto = 0,lines-1 DO BEGIN
          date_meto = data[0,i_meto]
          a_meto =STRTRIM(STRING(FLOOR(date_meto)),2)
          IF a_meto EQ mark THEN BEGIN
            b_meto = i_meto
          ENDIF ELSE BEGIN
            CONTINUE
          ENDELSE
          
          TEMP = data[1,b_meto]
          TMIN = data[3,b_meto]
          TMAX = data[2,b_meto]
          SUNT = data[4,b_meto]
          RAIN = data[5,b_meto]
          WIND = data[7,b_meto]
          VAPOUR = data[8,b_meto]
          RAIN = RAIN /10.0
          VAPOUR = VAPOUR /10.0
          TEMP = FLOAT(TEMP)
          TMIN = FLOAT(TMIN)
          TMAX = FLOAT(TMAX)
          SUNT = FLOAT(SUNT)
          RAIN = FLOAT(RAIN)
          WIND = FLOAT(WIND)
          VAPOUR = FLOAT(VAPOUR)
        ENDFOR
        RAINT = RAINT + RAIN
        ju = julday(date.month, date.day, date.year) - julday(12, 31, date.year - 1)
        ;------------------------------------------------------------------------------------------------------------
        ;  TEMP  = (TMIN+TMAX)/2.
        DTEMP = (TMAX+TEMP)/2.
        lim_min = 0
        lim_max = TEFFMX -TBASEM
        lim_x = TEMP -TBASEM
        DTSUME = CAL_LIMIT(lim_min,lim_max,lim_x)
        DTSUM = 0
        DVR = 0
        AF1 =  DTSMTB1
        AF2 =  DTSMTB2
        AF3 =  TEMP
        DTSUM = CAL_AFGEN(AF1,AF2,AF3)
        DVRED = 1
        
        IF DVS LT 1 THEN BEGIN
          DVR = DVRED*DTSUM/TSUM1
        ENDIF ELSE BEGIN
          DVR = DTSUM/TSUM2
        ENDELSE
        ;-----------------------------------------------------------------------------------------------------------------------
        ;每天生物量及叶面积指数的增量模拟（simulate the daily biomass and LAI increment）
        ;      FOR i_ns = 0,ns-1 DO BEGIN
        ;        FOR i_nl = 0,nl-1 DO BEGIN
        TADW = TADW_data
        NDVI_data = NDVI_data1
        latitude = latitude1
        ELEV = DEM1
        WRT = WRT_DATA
        WST = WST_DATA
        WSO = WSO_DATA
        WLV = WLV_DATA
        LAI = LAI_DATA
        LAISUM = LASUM_data
        LAIEXP = LAIEXP_data
        LAIMAX = LAIMAX_data
        ;-------------------------------------------------------------------------------------------------------------------
        ;判断是否需要按照作物分布进行模拟(The crop type file will be used or not)
        IF crop_cut EQ 1.0 THEN crop_cut1 = crop_cut_data
        ;          IF crop_cut EQ 1.0 AND crop_cut1 NE crop_type THEN BEGIN
        ;            LAIMAX = 0.0
        ;            LAIEXP = 0.0
        ;            LAIEXP_data = 0.0
        ;            GWSO_data = 0.0
        ;            GWRT_data = 0.0
        ;            GWST_data = 0.0
        ;            WLV_data = 0.0
        ;            LASUM_data = 0.0
        ;            LAIMAX_data = 0.0
        ;            REST_data = 0.0
        ;            AVARD_data = 0.0
        ;            DTGA_data = 0.0
        ;            LV_data = 0.0
        ;            SLAT_data = 0.0
        ;          ENDIF ELSE BEGIN
        ;-------------------------------------------------------------------------------------------------------------------
        ;利用光能利用率计算干物质累计量(simulate the daily biomass and LAI increment by CASA model)
        ;            Albedo = 0.526-0.443*NDVI_data
        ;            avard_casa,PGASS,ju, sunt, TEMP, NDVI_data, albedo, et_w, latitude, maxlue, sr_max, sr_min, c, optimalT,crop_type
        ;-------------------------------------------------------------------------------------------------------------------
        ;CO2同化模拟(CO2 assimilation)
        E_x = 1.0
        Declination  = 0.409 * SIN(2 * !PI * Ju / 365 - 1.405)
        wt = 2*!PI*(0.0667*(Longtitude-latitude))/24
        w_a = SIN(latitude)*SIN(Declination)
        w_b = COS(latitude)*COS(Declination)
        b_SunAngle = w_a + w_b * COS(wt)
        w_r = 1367*0.75*b_SunAngle                                                                           ;每日短波辐射(Daily shortwave radiation)
        E_f = 0.526-0.443*NDVI_data                                                                          ;反射率(reflectivity)
        w_rn =(1.0-E_f)*w_r*1.2+ E_x*5.6697*10.0^(-8)*TEMP - E_x*5.6697*10.0^(-8)*TEMP                       ;短波辐射功率(power of shortwave radiation)
        AVRAD =float(w_rn)*sunt *3.6*1000
        AVRAD  = float(AVRAD)
        AF1 = AMAXTB1
        AF2 = AMAXTB2
        AF3 = DVS
        AMAX = CAL_AFGEN(AF1,AF2,AF3)
        AF1 = TMPFTB1
        AF2 = TMPFTB2
        AF3 = DTEMP
        TMPFTB= CAL_AFGEN(AF1,AF2,AF3)
        AMAX = AMAX*TMPFTB
        AF1 = KDIFTB1
        AF2 = KDIFTB2
        AF3 = DVS
        KDIF = CAL_AFGEN(AF1,AF2,AF3)
        AF1 = EEFTB1
        AF2 = EEFTB2
        AF3 = DTEMP
        EFF = CAL_AFGEN(AF1,AF2,AF3)
        CO2_assimilation,PGASS,AMAX,EFF,LAI,KDIF,sunt,latitude,date,ju,NDVI_data,TEMP,AVRAD
        ;--------------------------------------------------------------------------------------------------------------------
        ;土壤水分模拟(Soil water simualtion)
        soil_simulation,I_stess,ISTATE,IWB,IOX,IAIRDU,KDIF,CFET,DEPNR,E0,ES0,ET0,LAI,CRAIRC,SM,SM0,SMW,SSI,RD,RDMCR,RIN,RAIN,NOTINF,DELT,EVSMX,K0,Ju,latitude,AVRAD,ELEV,ANGSTA,ANGSTB,TMIN,TMAX,TEMP,VAPOUR,WIND,RRI,RDMSOL,FR,IDEM,SM_t,RDI,TRA,EVWMX,RAINT,SMFCF,XDEF,IDRAIN,DD,SMTAB1,SMTAB2,CONTAB1,CONTAB2,SOPE,KSUB,ZTI
        ;--------------------------------------------------------------------------------------------------------------------
        ;作物蒸腾模拟(Crop transpiration simulation)
        transpiration_simulation,I_STESS,TRA,TRAMX,IWB,IOX,IAIRDU,KDIF,CFET,DEPNR,E0,ES0,ET0,LAI,CRAIRC,SM,SM0,SMFCF,SMW,SM_t,RD,RDI,RDRSTB1,RDRSTB2
        ;--------------------------------------------------------------------------------------------------------------------
        ;呼吸作用及干物质分配模拟,得到最终的生物量(Respiration and dry matter formation simulation to get the biomass)
        
        GASS = PGASS *TRA/TRAMX
        AF1 =  RFSETB1
        AF2 =  RFSETB2
        AF3 =  DVS
        RFSETB_data = CAL_AFGEN(AF1,AF2,AF3)
        RMRES = (RMR*WRT+RML*WLV+RMS*WST+RMO*WSO)*RFSETB_data
        TEFF  = Q10^((TEMP-25.)/10.)
        MRES  = GASS < (RMRES*TEFF)
        ;            MRES  = MIN([GASS, RMRES*TEFF])
        ASRC  = GASS-MRES                                                                                                               ;计算呼吸消耗(Respiration)
        AF1 =  FRTB1
        AF2 =  FRTB2
        FR = CAL_AFGEN(AF1,AF2,AF3)
        AF1 =  FLTB1
        AF2 =  FLTB2
        FL = CAL_AFGEN(AF1,AF2,AF3)
        AF1 =  FSTB1
        AF2 =  FSTB2
        FS = CAL_AFGEN(AF1,AF2,AF3)
        AF1 =  FOTB1
        AF2 =  FOTB2
        FO = CAL_AFGEN(AF1,AF2,AF3)
        CVF = 1./((FL/CVL+FS/CVS+FO/CVO)*(1.-FR)+FR/CVR)
        DMI = CVF*ASRC                                                                                                                   ;实际的干物质增量(Biomass increment)
        ADMI = (1.-FR)*DMI                                                                                                              ;;实际的地上部分干物质增量(above ground biomass increment)
        GRRT = FR*DMI
        AF1 =  RDRRTB1
        AF2 =  RDRRTB2
        DRRT = WRT*CAL_AFGEN(AF1,AF2,AF3)
        GWRT = GRRT-DRRT                                                                                                                ;根的增长速率(root growth rate)
        GRLV = FL*ADMI                                                                                                                  ;新增叶片的重量(new leaf increment)
        DSLV1 = WLV*(1.-TRA/TRAMX)*PERDL
        LAICR = 3.2/KDIF
        lim_min = 0
        lim_max = 0.03
        lim_x = 0.03*(LAI-LAICR)/LAICR
        DSLV2_DATA = CAL_LIMIT(lim_min,lim_max,lim_x)
        DSLV2 = WLV*DSLV2_DATA
        DSLV  = DSLV1 > DSLV2
        ;            DSLV  = MAX([DSLV1, DSLV2])
        REST = DSLV*DELT
        LV_1 = GRLV -REST
        LV_day = 0 > LV_1
        ;            LV_day = MAX([0,LV_1])
        AF3 = DVS
        AF1 = SLATB1
        AF2 = SLATB2
        SLAT= CAL_AFGEN(AF1,AF2,AF3)
        DTEFF = LAIEXP *0.0 
        GLAIEX = LAIEXP *0.0
        GLASOL = LAIEXP *0.0 
        GLA = LAIEXP *0.0
        SLAT = LAIEXP *0.0 +SLAT
        index_WF1=WHERE(LAIEXP LT 6.0 ,count_WF1)
        
        IF count_WF1 GT 0 THEN BEGIN
          DTEFF[index_WF1] = 0.> (TEMP-TBASE)
          ;              DTEFF = MAX([0.,TEMP-TBASE])
          GLAIEX[index_WF1]=LAIEXP[index_WF1]*RGRLAI*DTEFF[index_WF1]
          GLASOL[index_WF1] = GRLV[index_WF1]*SLAT[index_WF1]
          GLA[index_WF1] = GLAIEX[index_WF1] < GLASOL[index_WF1]
          ;              GLA = MIN([GLAIEX, GLASOL])
          ;        IF GRLV GT 0.0 THEN SLAT = GLA/GRLV
          index_WF2=WHERE(GRLV GT 0.0 ,count_WF2)
          IF count_WF2 GT 0 THEN SLAT[index_WF2] = GLA[index_WF2]/GRLV[index_WF2]
        ENDIF
        
        ;      IF LAIEXP LT 6.0 THEN BEGIN
        ;        DTEFF = 0.> (TEMP-TBASE)
        ;        ;              DTEFF = MAX([0.,TEMP-TBASE])
        ;        GLAIEX=LAIEXP*RGRLAI*DTEFF
        ;        GLASOL = GRLV*SLAT
        ;        GLA = GLAIEX < GLASOL
        ;        ;              GLA = MIN([GLAIEX, GLASOL])
        ;        IF GRLV GT 0.0 THEN SLAT = GLA/GRLV
        ;      ENDIF
        DSLVT = DSLV*DELT
        GRST = FS*ADMI
        AF1 = RDRSTB1
        AF2 = RDRSTB2
        DRST = CAL_AFGEN(AF1,AF2,AF3)*WST
        GWST = GRST-DRST                                                                                                              ;茎的增长速率(stem growth rate)
        GWSO = FO*ADMI                                                                                                                ;果实的增长速率(fruit growth rate)
        DRSO = 0.0
        ;---------------------------------------------------------------------------------------------------------------------------------
        ;          if  TSUME ge TSUMEM and ISTATE lt 3.0 then begin
        ;            WRT  = FR*TDWI
        ;            TADW = (1.-FR)*TDWI
        ;            WST_DATA  = WST_DATA *0.0 +FS*TADW
        ;            WSO_DATA  = WSO_DATA * 0.0 +FO*TADW
        ;            WLV_DATA    = WLV_DATA * 0.0 +FL*TADW
        ;            LAIEM_data  = WLV*mean(SLA[0,*,*])
        ;            LV_initial[*,*]  = latitude1 *0.0 +WLV_DATA
        ;            LASUM  = LAIEM_data
        ;            LAIEXP = LAIEM_data
        ;            LAIMAX = LAIEM_data
        ;            AF1 = SSATB1
        ;            AF2 = SSATB2
        ;            SSA = CAL_AFGEN(AF1,AF2,AF3)
        ;            LAI = LASUM+SSA*WST_DATA+SPA*WSO_DATA+0.04836
        ;            LAI_start = 1.0
        ;          endif
        ;--------------------------------------------------------------------------------------------------------------------------------
        ;汇总需要输出的参数（各部分生物量、叶面积指数等）（Gather the output parameter）
        LAIMAX = LAI > LAIMAX
        ;            LAIMAX = MAX([LAI,LAIMAX])
        LAIEXP = LAIEXP+GLAIEX*DELT
        LAIEXP_data = LAIEXP
        GWSO_data = GWSO
        GWRT_data = GWRT
        GWST_data = GWST
        WLV_data = WLV
        LASUM_data = LASUM
        LAIMAX_data  = LAIMAX
        die_wst = DRST*DELT
        die_wso = DRSO*DELT
        die_wrt = DRRT*DELT
        die_wlv = DSLV*DELT
        
        REST_data = REST
        AVARD_data = AVRAD
        DTGA_data = ADMI
        
        LV_data = GRLV
        SLAT_data = SLAT
        ;                ENDELSE
        ;        ENDFOR
        ;      ENDFOR
        ;----------------------------------------------------------------------------------------------------------------------------------
        ;通过积温计算是否出苗(Judge the emerge time)
        IF  TSUME GE TSUMEM AND ISTATE LT 3.0 THEN BEGIN
          WRT  = FR*TDWI
          TADW = (1.-FR)*TDWI
          WST_DATA  = WST_DATA *0.0 +FS*TADW
          WSO_DATA  = WSO_DATA * 0.0 +FO*TADW
          WLV_DATA    = WLV_DATA * 0.0 +FL*TADW
          cc = WST_data *0.0
          cc = REFORM(SLA[0,*,*])
          LAIEM_data = WLV_DATA*cc
          LV_initial[*,*]  = latitude1 *0.0 +WLV_DATA
          LASUM_data  = LAIEM_data
          LAIEXP_data = LAIEM_data
          LAIMAX_data = LAIEM_data
          AF1 = SSATB1
          AF2 = SSATB2
          SSA = CAL_AFGEN(AF1,AF2,AF3)
          LAI_data = LASUM_data+SSA*WST_DATA+SPA*WSO_DATA+0.04836
          LAI_start = 1.0
        ENDIF
        ;-----------------------------------------------------------------------------------------------------------------------------------------
        ;生育期及积温计算(Growth period and accumulated temperature calculation)
        IF ISTATE LT 3.0 AND TSUME GE TSUMEM THEN ISTATE = 3.0
        IF ISTATE LT 3.0 THEN TSUME = TSUME+DTSUME*DELT
        ;----------------------------------------------------------------------------------------------------------------------------------
        ;叶片模拟部分,注释部分为WOFOST7.1的原代码(Leaf simulation, the annotation contents are the source code of WOFOST7.1)
        IF LAI_start EQ 1.0 THEN BEGIN
          I1   = ILVOLD
          LV[0,*,*] = LV_data
          ;        while REST gt LV[I1-1] and I1 ge 1 do begin
          ;          REST = REST -LV[I1-1]
          ;          I1 = I1-1
          ;          continue
          ;        endwhile
          ;    REST_mean = max(REST_data)
          DSLV_data = REST_data / DELT
          WHILE LVAGE[I1-1] GT SPAN  AND I1 GE 1 DO BEGIN
            DALV = DALV+LV[I1-1,*,*]
            I1 = I1-1
            CONTINUE
          ENDWHILE
          die_wlv = die_wlv +DALV*DELT
          I1   = ILVOLD
          FOR J_lv = 1 , I1 DO BEGIN
          
            DSLV_mean = mean(DSLV_data)
            IF DSLV_mean GT 0.0 AND I1 GE 1 THEN BEGIN
              LV_mean = mean(LV[I1-1,*,*])
              IF DSLV_mean GE LV_mean THEN BEGIN
                DSLV_data = DSLV_data - LV[I1-1,*,*]
                LV[I1-1,*,*] = DSLV_data *0.0
                I1 = I1-1
              ENDIF ELSE BEGIN
                LV[I1-1,*,*] = LV[I1-1,*,*] - DSLV_data
                DSLV_data = DSLV_data *0.0
              ENDELSE
            ENDIF
          ENDFOR
          FOR J_lv = 1,I1 DO BEGIN
            IF  LVAGE[I1-1] GT SPAN AND I1 GE 1 THEN BEGIN
              LV[I1-1,*,*] = LV[I1-1,*,*] *0.0
              I1 = I1-1
            ENDIF
          ENDFOR
          ;
          ;        while LVAGE[I1-1] gt SPAN  and I1 ge 1 do begin
          ;          DALV = DALV+LV[I1-1,*,*]
          ;          I1 = I1-1
          ;          continue
          ;        endwhile
          ;        DALV = DALV/DELT
          ;
          ;        while DSLVT gt 0 and I1 ge 1 do begin
          ;          if DSLVT ge LV[I1-1] then begin
          ;            DSLVT  = DSLVT-LV(I1-1)
          ;            LV[I1-1] = 0.
          ;            I1 = I1-1
          ;          endif else begin
          ;            LV[I1-1] = LV[I1-1] -DSLVT
          ;            DSLVT = 0
          ;          endelse
          ;          continue
          ;        endwhile
          ;        while LVAGE[I1-1] ge SPAN and I1 ge 1 do begin
          ;          LV[I1-1,*,*] = LV_data * 0.0
          ;          I1 = I1-1
          ;          continue
          ;        endwhile
          ILVOLD = I1
          FYSDEL = 0.0 > ((TEMP-TBASE)/(35.-TBASE))
          ;        FYSDEL = MAX([0.,(TEMP-TBASE)/(35.-TBASE)])
          FOR I1 = ILVOLD,1,-1 DO BEGIN
            LV[I1,*,*] = LV[I1-1,*,*]
            SLA[I1,*,*] = SLA[I1-1,*,*]
            LVAGE[I1] = LVAGE[I1-1]+FYSDEL*DELT
          ENDFOR
          I1 = I1 +1
          ILVOLD = ILVOLD +1
          ;    LV[0,*,*]= GRLV*DELT                                                                                        
          SLA[0,*,*]   = SLAT_data
          LVAGE[0] = 0.
          WLV_DATA = WLV_DATA *0.0 + LV_initial[*,*]
          LASUM_data = LASUM_data*0.0 + 0.04836
          FOR I1 = 1,ILVOLD DO BEGIN
            LASUM_data = LASUM_data+LV[I1-1,*,*]*SLA[I1-1,*,*]
            WLV_data  = WLV_data + LV[I1-1,*,*]                                                                             
          ENDFOR
          I1 = I1 -1
          LAI_data = LASUM_data+SSA*WST+SPA*WSO
;          print,'s'
        ENDIF
        ;-----------------------------------------------------------------------------------------------------------------------
        ;;利用遥感数据计算叶面积指数(LAI caculation by RS data through specific model)
        ;Please instead by the new model in your study area
        LAI_RSMX = 0.0
        LAI_RSMN = 7.0
        LAI_RST = 0.0
        LAI_DN = 0.0
        LAI_RSLS = 0.0
        LAI_WFT = 0.0
        LAI_COUNT = 0.0
        crop_cut_rs = 0
        LAIWF_check = 0.0
        LAIRS_check = 0.0
        LAIRS_checkmx = 0.0
        LAIRS_checkmn = 0.0       
        NDVI_RS = NDVI_data1  
     
        ;------------------------------------------------------------------------------------------------------
        ;please apply a RS-model for LAI estimation like below(请在下面部分提供基于遥感数据的LAI计算模型)
        index_LAIRS1=WHERE(NDVI_RS LE 0.15,count_LAIRS1)
        IF count_LAIRS1 GT 0 THEN BEGIN
          LAI_RS[index_LAIRS1] = 0.0
        ENDIF
        index_LAIRS2=WHERE(NDVI_RS GT 0.15,count_LAIRS2)
        
        IF count_LAIRS2 GT 0 THEN BEGIN
          IF crop_type EQ 1 THEN BEGIN
            IF DVS LT 1.0 THEN LAI_RS[index_LAIRS2] = 5.8278*NDVI_RS[index_LAIRS2] - 0.784
            IF DVS GE 1.0 THEN LAI_RS[index_LAIRS2] = 4.5640*NDVI_RS[index_LAIRS2] + 0.026
          ENDIF
          IF crop_type EQ 2 THEN BEGIN
            IF DVS LT 1.2 THEN LAI_RS[index_LAIRS2] = 5.1303*NDVI_RS[index_LAIRS2] - 0.7932
            IF DVS GE 1.2 THEN LAI_RS[index_LAIRS2] = 2.9849*NDVI_RS[index_LAIRS2] + 1.2028
          ENDIF
          IF crop_type EQ 3 THEN  LAI_RS[index_LAIRS2] = -3.891*NDVI_RS[index_LAIRS2]*NDVI_RS[index_LAIRS2] +6.812 *NDVI_RS[index_LAIRS2] +0.175
          IF crop_type EQ 0 THEN  LAI_RS[index_LAIRS2] = -4.751*NDVI_RS[index_LAIRS2]*NDVI_RS[index_LAIRS2] +8.605 *NDVI_RS[index_LAIRS2] -0.331
        ENDIF
        
        ;--------------------------------------------------------------------------------------------
         ;检测异常值(check the abnormal value), this part can be deleted if unnecessary
                
        LAIWF_check = LAI_data
        LAIRS_check = LAI_RS
        LAIRS_checkmx = LAIRS_check*3.8
        LAIRS_checkmn = LAIRS_check*0.95
        
        index_LAIRS3=WHERE(LAIWF_check LT LAIRS_checkmn AND DVS GT 0.4,count_LAIRS3)
        IF count_LAIRS3 GT 0 THEN BEGIN
          LAI_data[index_LAIRS3] = LAI_RS[index_LAIRS3]
        ENDIF
        index_LAIRS4=WHERE(LAIWF_check GE LAIRS_checkmx AND DVS GT 0.8,count_LAIRS4)
        IF count_LAIRS4 GT 0 THEN BEGIN
          LAIWF_check[index_LAIRS4] =(LAIWF_check[index_LAIRS4] +LAIRS_check[index_LAIRS4])/2.0
          
          index_LAIRS5=WHERE(LAIWF_check[index_LAIRS4] GE LAIRS_checkmx[index_LAIRS4],count_LAIRS5)
          
          IF count_LAIRS5 GT 0 THEN BEGIN
            index_LAIRS6 = index_LAIRS4[index_LAIRS5]
            LAIWF_check[index_LAIRS6] =(LAIWF_check[index_LAIRS6] +LAIRS_check[index_LAIRS6])/2.0
          ENDIF
          LAI_data[index_LAIRS4] = LAIWF_check[index_LAIRS4]
        ENDIF
        ;-----------------------------------------------------        
        ; 遥感数据同化方法(Assimilation method)
        ; Part1
        ; 新同化方法（The new assimilation method）
        ;Please write the assimilation method in this place
        ;According to relevant references:1.  Zhiqiang Cheng, Jihua Meng, and Yiming Wang. 
        ;Improving Spring Maize Yield Estimation at Field Scale by Assimilating Time-Series HJ-1 CCD Data into the WOFOST Model Using a New Method with Fast Algorithms[J].
        ;Remote Sensing, 2016, 8(4), 303.
        ;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        ;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        ;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        ;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        ;
        ;--------------------------------------------------------------------------------------------------------
        ;Part2
        ;直接驱动法(Direct drive method of assimilation)
        ;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        ;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        ;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        ;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        ;--------------------------------------------------------------------------------------------------------
        ;Part3
        ;集合卡尔曼滤波法(EnKF method)
        ;--------------------------------------------------------------------------------------------------------
        ;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        ;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        
        
        
        ;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        ;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        ;---------------------------------------------------------------------------------------------------------
        WRT_DATA  = WRT_DATA+GWRT_data*DELT
        WST_DATA  = WST_DATA+GWST_data*DELT
        WSO_DATA  = WSO_DATA+GWSO_data*DELT
        TADW_data = WLV_DATA+WST_DATA+WSO_DATA
        die_twso = die_twso+die_wso                                                                                        ; 作物死亡部分(The death biomass)
        die_twst = die_twst+die_wst
        die_twlv = die_twlv+die_wlv
        die_twrt = die_twrt+die_wrt
        TWST_DATA1 = die_twst+WST_DATA                                                                                     ;总生物量，包括死亡部分(The total biomass)
        TWLV_DATA1 = die_twlv+WLV_data
        TWSO_DATA1 = die_twso+WSO_DATA
        TWRT_DATA1 = die_twrt+WRT_DATA
        Tbio = TWST_DATA1 + TWLV_DATA1 + TWSO_DATA1 + TWRT_DATA1
        ;    TAGP = TWLV+TWST+TWSO
        ;    GASST = GASS + GASST                                                                                           ;总同化量(The tatal assimilation amount)
        ;    MREST = MRES + MREST                                                                                           ;总呼吸消耗量(The total respiration amount)
        AF1 = SSATB1
        AF2 = SSATB2
        SSA = CAL_AFGEN(AF1,AF2,AF3)
        
        ;    TRAT = TRA + TRAT                                                                                              ;总的蒸散量(The total evapotranspire amount)
        IDOST = IDOST + IDOS
        IDWST = IDWST + IDWS
        IDOSJ = IDOSJ + IDOS
        IDWSJ = IDWSJ + IDWS
        bio_total = WST_DATA  + WSO_DATA + WLV_data
        bio_data = bio_total
        
        IF DVS GE 1 AND IDANTH EQ -99 THEN BEGIN
          IDANTH = ju - ju1
          DVS = 1
          NOANTH = 1
        ENDIF
        TSUME = TSUME+DTSUME*DELT
        DVS  = DVS+DVR*DELT
        TSUM = TSUM+DTSUM*DELT
        J_MODULE1 = 0
        FOR J_MODULE = nl_begin,nl_end DO BEGIN
          lai_total[*,J_MODULE] = LAI_data[*,J_MODULE1]
          bio_total_net[*,J_MODULE] = bio_data[*,J_MODULE1]
          J_MODULE1 = J_MODULE1+1
        ENDFOR
        ;        bio_total = FLTARR(ns,nl_total)
        ;        yield_total = FLTARR(ns,nl_total)
        ;        yield_total_nu = FLTARR(ns,nl_total)
        ;        lai_total = FLTARR(ns,nl_total)
        ;        bio_total_total = FLTARR(ns,nl_total)
        
        WRITE_TIFF,out_bio_temporary  + Q_block_name+ 'hongxing_' + mark +'_bio.tif',bio_total_net,geotiff = geotiff, /float
        WRITE_TIFF,out_LAI_temporary + Q_block_name + 'hongxing_' + mark + '_LAI.tif',lai_total,geotiff = geotiff, /float
        LAI_1 = MAX(LAI_data)
        IF ILVOLD GT 365 OR DVS GE DVSEND OR (LAI_1 LE 0.002 AND DVS GT 0.5) THEN BEGIN
          TERMNL = 1
          DUROPT = j_stday-Ju1
          i_NDVI3 = NDVI_count
        ENDIF
        ;------------------------------------------------------------------------------------------------------
        IF IWB EQ 0 THEN BEGIN
          RATIO  = TWSO/(TWLV+TWST)
          HINDEX = TWSO/TAGP
          TRC    = 100000.*TRAT/TAGP
        ENDIF
      ENDELSE
    ENDFOR
    FREE_LUN,lun
    
    
    ;生育期模拟结束(The end of crop growth simulation)
    ;----------------------------------------------------------------------------------------------------------
    ;输出潜在产量或者水分限制产量(The potential and  water-limited yield output )
    IF crop_type EQ 1 THEN cropout = 'maize'
    IF crop_type EQ 2 THEN cropout = 'soybean'
    IF crop_type EQ 3 THEN cropout = 'wheat'
    IF crop_type EQ 0 THEN cropout = 'other'
    yield_data = WSO_DATA
    J_MODULE1 = 0
    FOR J_MODULE = nl_begin,nl_end DO BEGIN
      yield_total[*,J_MODULE] = yield_data[*,J_MODULE1]
      J_MODULE1 = J_MODULE1+1
    ENDFOR
    WRITE_TIFF,out_yield_temporary  + Q_block_name + 'hongxing_' + mark  +'_' +cropout +'_water_yield.tif',yield_total,geotiff = geotiff, /float
    ;CCC = CCC +'A'
    ;WRITE_TIFF,out_yield + 'hongxing_' + mark + '_'+ CCC +'_' +cropout +'_yield.tif',yield_data,geotiff = geotiff, /float
    ;----------------------------------------------------------------------------------------------------------
    ;养分限制产量模拟（The nutrient-limited yield simulation and output）
    if I_nutrient eq 1 then begin
      DUROPT = j_stday-Ju1
      yield_NUTRIE,yield_nu,DUROPT,WSO_DATA,WST_DATA,WLV_data,ns,nl,crop_type,TWST_DATA1,TWLV_DATA1,TWSO_DATA1,TWRT_DATA1,RD,parameter_txt_path
      J_MODULE1 = 0
      FOR J_MODULE = nl_begin,nl_end DO BEGIN
        yield_total_nu[*,J_MODULE] = yield_nu[*,J_MODULE1]
        J_MODULE1 = J_MODULE1+1
      ENDFOR
      WRITE_TIFF,out_yield_temporary + Q_block_name + 'hongxing_' + mark  +'_' + cropout +'_nutrient_yield.tif',yield_total_nu,geotiff = geotiff, /float
    endif
    ;----------------------------------------------------------------------------------------------------------
    J_MODULE1 = 0
    FOR J_MODULE = nl_begin,nl_end DO BEGIN
      bio_total_total[*,J_MODULE] = Tbio[*,J_MODULE1]
      J_MODULE1 = J_MODULE1+1
    ENDFOR
    
    WRITE_TIFF,out_yield_temporary + Q_block_name + 'hongxing_' + mark + '_' +cropout +'_total_bio.tif',bio_total_total,geotiff = geotiff, /float
    
    WIDGET_CONTROL,tlb,/destroy
  ENDFOR
  ;--------------------------------------------------------------------------------------------------------------------------------------------
  WRITE_TIFF,out_yield + 'hongxing_' + mark + '_' +cropout +'_total_bio.tif',bio_total_total,geotiff = geotiff, /float
  WRITE_TIFF,out_yield + 'hongxing_' + mark + '_' +cropout +'_nutrient_yield.tif',yield_total_nu,geotiff = geotiff, /float
  WRITE_TIFF,out_yield + 'hongxing_' + mark + '_' +cropout +'_water_yield.tif',yield_total,geotiff = geotiff, /float
  ;ENDFOR
  file_delete1 = FILE_SEARCH(NDVI_file + '*_second.tif')
  FILE_DELETE,file_delete1
  message=''  

END

