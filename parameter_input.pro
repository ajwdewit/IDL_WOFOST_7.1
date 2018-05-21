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
;----------------------------------------------------------------------------------------
;生成模型运行所需要的参数列表返回主工程模块(Accept the necessary parameter and return to the main projiect, 
;corresponds to the AFGEN.FOR module of orginal WOFOST in fortran
;The meaning of these parameters can be find in spring maize module)

FUNCTION CAL_LIMIT ,lim_min,lim_max,lim_x
lim_max = lim_x *0.0 +lim_max
lim_min = lim_min *0.0 +lim_min

limit = lim_x *0.0
  index_lim1=WHERE(lim_x LT lim_min,count_lim1)
  IF count_lim1 GT 0 THEN BEGIN
  limit[index_lim1] = lim_min[index_lim1]
  ENDIF
    index_lim2=WHERE(lim_x GE lim_min AND lim_x LE lim_max ,count_lim2)
  IF count_lim2 GT 0 THEN BEGIN
  limit[index_lim2] = lim_x[index_lim2]
  ENDIF
    index_lim3=WHERE(lim_x GE lim_min AND lim_x GT lim_max,count_lim3)
  IF count_lim3 GT 0 THEN BEGIN
  limit[index_lim3] = lim_max[index_lim3]
  ENDIF
  
  index_lim4=WHERE(lim_max LT lim_min,count_lim4)
  IF count_lim4 GT 0 THEN BEGIN
  limit[index_lim4] = 0
  ENDIF

;  IF lim_max LT lim_min THEN BEGIN
;    limit = 0
;  ENDIF ELSE BEGIN
;    IF lim_x LT lim_min THEN BEGIN
;      limit = lim_min
;    ENDIF ELSE BEGIN
;      IF lim_x LE lim_max THEN BEGIN
;        limit = lim_x
;      ENDIF ELSE BEGIN
;        limit = lim_max
;      ENDELSE
;    ENDELSE
;  ENDELSE
  RETURN,limit
END
FUNCTION CAL_AFGEN,AF1,AF2,AF3
  j = 0
  AF_num = N_ELEMENTS(AF1)
  IF AF1[0] GE AF3 THEN BEGIN
    AFGEN = AF2[0]
  ENDIF ELSE BEGIN
    FOR i = 1,AF_num-1 DO BEGIN
      IF AF1[i] GE AF3 THEN BEGIN
        slope = (AF2[i]-AF2[i-1])/(AF1[i]-AF1[i-1])
        AFGEN = AF2[i-1] + (AF3 - AF1[i-1])*slope
        j = j +1
        BREAK
      ENDIF ELSE BEGIN
        IF AF1[i] LT AF1[i-1] THEN BEGIN
          AFGEN = AF2[i-1]
          BREAK
        ENDIF ELSE BEGIN
          CONTINUE
        ENDELSE
      ENDELSE
    ENDFOR
    i = i -1
    IF j EQ 0 THEN BEGIN
      AFGEN = AF2[i]
    ENDIF ELSE BEGIN
      AFGEN  = AFGEN
    ENDELSE
  ENDELSE
  RETURN, AFGEN
END

PRO parameter_input,crop_type,parameter_txt_path,DELT,IWB,FLTB1,FLTB2,TEFFMX,CRAIRC,I_nutrient,I_stess,IOX,IAIRDU,i_stday,stday_start,stday_end,DEPNR,TSUM1,TSUM2,SMFCF,SMW,DVS,SM0,RMR,RMO,RML,RMS,Q10,CVL,CVO,CVR,CVS,TDWI,Ju1,DVSEND,SPAN,PERDL,TBASE,RDI,RRI,SPA,RDMCR,RDMSOL,RGRLAI,ISTATE,LASUM,RAINT,IDEM,K0,LAI_start,TSUMEM,NOTINF,SM_t,SMSLATB1,TBASEM,CFET,SPADS,SPODS,SPASS,SPOSS,SLATB1,SLATB2,SSATB1,SSATB2,FRTB1,FRTB2,FSTB1,FSTB2,FOTB1,FOTB2,DTSMTB1,DTSMTB2,AMAXTB1,AMAXTB2,TMPFTB1,TMPFTB2,EEFTB1,EEFTB2,KDIFTB1,KDIFTB2,RFSETB1,RFSETB2,RDRSTB1,RDRSTB2,RDRRTB1,RDRRTB2
  
  ;--------------------------------------------------------------------------------------------------------------------------------------
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
  IF crop_type EQ 1 THEN BEGIN
  ;------------------------------------------------------------------------------------------------
  ;输入玉米相关参数（Spring maize input parameters）
      DELT = 1                       ;模拟的时间步长 （Simulation step）
      IWB = 1                        ;是否模拟水分胁迫，(Whether simulate water stress)1,Y;0, N。
      I_nutrient = 1                 ;是否模拟养分胁迫，(Whether simulate nutrient stress)1,Y;0,N
      I_stess = 2                    ;模拟水分胁迫的方式, (The way the water stress can be simulated)0 for no,1 for without groundwater,2 for with groundwater
      IOX = 1                        ;是否模拟根系缺氧(Whether the oxygen deficit in root system is considered)，1,Y;0, N。
      IAIRDU = 0                     ;根系中的通风管道(Whether air ducts in roots present )，0,N;1, Y。
      i_stday = 1.0                  ;是否已知播种时间(Whether the sow time is known)，1,Y;0, N。
      stday_start = 100              ;未知播种时间时的播种时间范围(The time horizon of sow time if it cannot be determined)
      stday_end = 140
      DEPNR = 4                      ;作物对水的敏感性（Crop group number for soil water depletion）
      TSUM1 = 940                    ;作物从出苗到开花需要的积温(temperature sum from emergence to anthesis)
      TSUM2 = 650                    ;从开花到成熟所需要的积温(temperature sum from anthesis to maturity)
      SMFCF = 0.460                  ;田间持水量(soil moisture content at field capacity)
      SMW = 0.200                    ;凋萎点田间含水量含水量(Soil moisture content at wilting point)
      SM0 = 0.570                    ;饱和田间含水量(Soil moisture content at saturation)
      TBASEM = 5                     ;出苗所需的最低温度(Lower threshold temp. for emergence)
      TEFFMX =30                     ;出苗所需的最高温度(Max. eff. temp. for emergence)
      CRAIRC = 0.075                 ;土壤中的空气含量(Critical soil air content for aeration)
      RMR = 0.010                    ;标准温度下的维持呼吸消耗量(Rel. maint. resp. rate)-root
      RMO = 0.009                    ;-storage
      RML = 0.025                    ;-leaf
      RMS = 0.015                    ;-stem
      Q10 = 2.0                      ;rel. incr. in resp. rate per 10 Cel temp. incr
      CVL = 0.690                    ;各个部分的呼吸效率(efficiency of conversion)-leaf
      CVO = 0.820                    ;-storage
      CVR = 0.720                    ;-root
      CVS = 0.750                    ;-stem
      TDWI = 110                     ;最初的生物量(Initial total crop dry weight)
      PERDL = 0.030                  ;叶片的凋亡系数(Max. rel. death rate of leaves due to water stress)
      Ju1 = 132                      ;播种日期(Sow time)
      DVSEND = 2.0                   ;收获期的DVS值(Development stage at harvest)
      SPAN = 23                      ;在35度时叶片的存活天数(Life span of leaves growing at 35 Celsius)
      TBASE = 10.0                   ;引起叶片衰老的最低温度(Lower threshold temp. for ageing of leaves)
      RDI  = 10                      ;初始根系深度(Initial rooting depth)
      RRI  = 2.2                     ;最大根系生长速度(Maximum daily increase in rooting depth)
      SPA = 0.0                      ;果实的特定叶面积，果实由重量到面积的转化系数,由DVS决定(Specific pod area)
      RDMCR = 100                    ;根系最大深度(maximum rooting depth)
      RDMSOL = 200                   ;土壤允许的最大根系深度(Maximum rooting depth allowed by soil)
      RGRLAI = 0.0294                ;叶面积指数的最大相关增长率(maximum relative increase in LAI)
      ISTATE = 2                     ;;判断出苗期(Judge the sow time)，1-2 NO； 3-4 YES 1-2 NO锟斤拷 3-4 YES
      LASUM = 0.0                    ;播种时的叶面积指数累计值(LAI at the sow time)
      RAINT = 0.0                    ;总降水量(Total rain amount)
      IDEM = 115                     ;作物出苗时间(Day of emergence)
      K0 = 10.789                    ;土壤饱和导水率(hydraulic conductivity of saturated soil)
      LAI_start = 0.0                ;开始模拟叶片生长的标志(Index of the starting of LAI simulation)
      TSUMEM = 110                   ;作物出苗所需要的积温(Temperature sum from sowing to emergence)
      NOTINF = 0.0                   ;降水量的地表流失量((Maximum) non-infiltrating fraction of rainfall)
      SM_t = 0.300                   ;最初的土壤含水量(The initial soil moisture content)
      SM = 0.300                     ;土壤含水量(The soil moisture content)
      CFET = 1.0                     ;作物蒸散的校正系数(Correction factor transpiration rate)
      SPADS    =   0.050             ;;苗床深度的渗透系数(Topsoil seepage parameter deep seedbed)
      SPODS    =   0.025
      SPASS    =   0.100
      SPOSS    =   0.040
      DEFLIM   =  -0.300
      TERMNL = 0
      DVSI = 0.0
      SMW = 0.099
      DVS = DVSI
      TDWI = 100
      TSUM   = 0.
      IDANTH = -99
      ;LV = FLTARR(100)
      LVAGE = FLTARR(100)
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
      SLATB1 = [0.00,0.58,2.00]                                               ;叶片由重量到面积的转化系数,由DVS决定(Specific leaf area as a function of DVS )
      SLATB2 = [0.0026,0.0012,0.0012]
      SSATB1 = [0.0,2.0]                                                      ;茎由重量到面积的转化系数,由DVS决定(Specific stem area as a function of DVS )
      SSATB2 = [0.0,0.0]
      FRTB1 = [0.00,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00,2.00]   ;干物质转化为根的转化系数，由DVS决定(Fraction of total dry matter to roots as a function of DVS )
      FRTB2 = [0.40,0.37,0.34,0.31,0.27,0.23,0.19,0.15,0.10,0.06,0.00,0.00]
      FLTB1 = [0.00,0.28,0.88,0.95,1.10,1.40,2.00]                            ;地上干物质转化为叶的转化系数，由DVS决定(Fraction of above-gr. DM  to leaves as a function of DVS )
      FLTB2 = [0.68,0.68,0.15,0.15,0.20,0.15,0.10]
      FSTB1 = [0.00,0.33,0.88,0.95,1.10,1.40,2.00]                            ;地上干物质转化为茎的转化系数，由DVS决定(Fraction of above-gr. DM to stems as a function of DVS )
      FSTB2 = [0.32,0.24,0.65,0.55,0.50,0.00,0.00]
      FOTB1 = [0.0,0.33,0.88,0.95,1.10,1.40,2.00]                             ;地上干物质转化为存储器官的转化系数，由DVS决定(Fraction of above-gr. DM to storages as a function of DVS )
      FOTB2 = [0.00,0.08,0.1,0.20,0.30,0.85,0.90]
      DTSMTB1 = [0.00,6.00,30.00,35.00]                                       ;每天temp. sum的增长量(Daily increase in temp. sum as a function of DVS)
      DTSMTB2 = [0.00,0.00,24.00,24.00]
      AMAXTB1 =[0.00,1.25,1.50,1.75,2.00]                                     ;最大CO2的同化系数(Max. leaf CO2 assim. rate as a function of DVS)
      AMAXTB2 =[75.00,75.00,63.00,55.00,25.00]
      TMPFTB1 = [0.00,9.00,16.00,18.00,20.00,30.00,36.00,42.00]               ;AMAX的衰减系数(Reduction factor of AMAX as function of av. temp.)
      TMPFTB2 = [0.01,0.05,0.80,0.94,1.00,1.00,0.95,0.56]
      EEFTB1 = [0,40]                                                         ;单一叶片的光能吸收系数(Light-use effic. single leaf )
      EEFTB2 = [0.45,0.45]
      KDIFTB1 = [0.0,2.0]                                                     ;可见光的消光系数(Extinction coefficient for diffuse visible light as a function of DVS)
      KDIFTB2 = [0.6,0.6]
      RFSETB1 = [0.0,1.5,1.75,2.0]                                            ;呼吸消耗随生育期的衰减系数(Red. factor for senescence as a function of DVS)
      RFSETB2 = [1.0,1.0,0.75,0.25]
      RDRSTB1 = [0.00,1.50,1.50001,2.00]                                      ;茎的死亡系数(Rel. death rate of roots as a function of DVS)
      RDRSTB2 = [0.000,0.000,0.0150,0.020]
      RDRRTB1 = [0.00000,1.50000,1.50001,2.00000]                             ;根的死亡系数(Rel. death rate of stems as a function of DVS)
      RDRRTB2 = [0.000,0.000,0.015,0.020]
      ;-------------------------------------------------------------------------------------------------------------------------
      ;玉米作物参数读取界面(The specific parameter input for spring maize)
    ;,SSI,XDEF,ZTI,IDRAIN,DD,SMTAB1,SMTAB2,CONTAB1,CONTAB2,SOPE,KSUB
    parameter_txt = parameter_txt_path + 'MAIZE_hongxing.txt'
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
      
      
      
      if mark2 eq 'DELT' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        DELT = FLOAT(para_data)
      endif
      if mark2 eq 'IWB' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        IWB = FLOAT(para_data)
      endif
      if mark2 eq 'I_nutrient' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        I_nutrient = FLOAT(para_data)
      endif
      if mark2 eq 'I_stess' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        I_stess = FLOAT(para_data)
      endif
      if mark2 eq 'IOX' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        IOX = FLOAT(para_data)
      endif
      if mark2 eq 'IAIRDU' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        IAIRDU = FLOAT(para_data)
      endif
      if mark2 eq 'i_stday' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        i_stday = FLOAT(para_data)
      endif
      if mark2 eq 'stday_start' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        stday_start = FLOAT(para_data)
      endif
      if mark2 eq 'stday_end' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        stday_end = FLOAT(para_data)
      endif
      if mark2 eq 'DEPNR' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        DEPNR = FLOAT(para_data)
      endif
      if mark2 eq 'TSUM1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TSUM1 = FLOAT(para_data)
      endif
      if mark2 eq 'TSUM2' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TSUM2 = FLOAT(para_data)
      endif
      if mark2 eq 'SMFCF' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SMFCF = FLOAT(para_data)
      endif
      if mark2 eq 'SMW' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SMW = FLOAT(para_data)
      endif
      if mark2 eq 'SM0' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SM0 = FLOAT(para_data)
      endif
      if mark2 eq 'TBASEM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TBASEM = FLOAT(para_data)
      endif
      if mark2 eq 'TEFFMX' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TEFFMX = FLOAT(para_data)
      endif
      if mark2 eq 'CRAIRC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CRAIRC = FLOAT(para_data)
      endif
      if mark2 eq 'RMR' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RMR = FLOAT(para_data)
      endif
      if mark2 eq 'RMO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RMO = FLOAT(para_data)
      endif
      if mark2 eq 'RML' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RML = FLOAT(para_data)
      endif
      if mark2 eq 'RMS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RMS = FLOAT(para_data)
      endif
      if mark2 eq 'Q10' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        Q10 = FLOAT(para_data)
      endif
      if mark2 eq 'CVL' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CVL = FLOAT(para_data)
      endif
          if mark2 eq 'CVO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CVO = FLOAT(para_data)
      endif
      if mark2 eq 'CVR' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CVR = FLOAT(para_data)
      endif
      if mark2 eq 'CVS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CVS = FLOAT(para_data)
      endif
      if mark2 eq 'TDWI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TDWI = FLOAT(para_data)
      endif
      if mark2 eq 'PERDL' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PERDL = FLOAT(para_data)
      endif
      if mark2 eq 'Ju1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        Ju1 = FLOAT(para_data)
      endif
      if mark2 eq 'DVSEND' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        DVSEND = FLOAT(para_data)
      endif
      if mark2 eq 'SPAN' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPAN = FLOAT(para_data)
      endif
      if mark2 eq 'TBASE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TBASE = FLOAT(para_data)
      endif
      if mark2 eq 'RDI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RDI = FLOAT(para_data)
      endif
      if mark2 eq 'RRI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RRI = FLOAT(para_data)
      endif
      if mark2 eq 'SPA' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPA = FLOAT(para_data)
      endif
      if mark2 eq 'RDMCR' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RDMCR = FLOAT(para_data)
      endif
      if mark2 eq 'RDMSOL' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RDMSOL = FLOAT(para_data)
      endif
      if mark2 eq 'RGRLAI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RGRLAI = FLOAT(para_data)
      endif
      if mark2 eq 'ISTATE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        ISTATE = FLOAT(para_data)
      endif
      if mark2 eq 'LASUM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        LASUM = FLOAT(para_data)
      endif
      if mark2 eq 'RAINT' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RAINT = FLOAT(para_data)
      endif
      if mark2 eq 'IDEM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        IDEM = FLOAT(para_data)
      endif
      if mark2 eq 'K0' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        K0 = FLOAT(para_data)
      endif
      if mark2 eq 'LAI_start' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        LAI_start = FLOAT(para_data)
      endif
      if mark2 eq 'TSUMEM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TSUMEM = FLOAT(para_data)
      endif
      if mark2 eq 'NOTINF' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NOTINF = FLOAT(para_data)
      endif
      if mark2 eq 'SM_t' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SM_t = FLOAT(para_data)
      endif
      if mark2 eq 'SM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SM = FLOAT(para_data)
      endif
      if mark2 eq 'CFET' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CFET = FLOAT(para_data)
      endif
      if mark2 eq 'SPADS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPADS = FLOAT(para_data)
      endif
      if mark2 eq 'SPODS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPODS = FLOAT(para_data)
      endif
      if mark2 eq 'SPASS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPASS = FLOAT(para_data)
      endif
      if mark2 eq 'SPOSS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPOSS = FLOAT(para_data)
      endif
      if mark2 eq 'DEFLIM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        DEFLIM = FLOAT(para_data)
      endif
      if mark2 eq 'TERMNL' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TERMNL = FLOAT(para_data)
      endif
      if mark2 eq 'DVSI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        DVSI = FLOAT(para_data)
      endif
      if mark2 eq 'SMW' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SMW = FLOAT(para_data)
      endif
      if mark2 eq 'TSUM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TSUM = FLOAT(para_data)
      endif
      if mark2 eq 'IDANTH' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        IDANTH = FLOAT(para_data)
      endif
      
      if mark2 eq 'SLATB1' then begin
        ;      SLATB1 = fltarr(3)
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        SLATB1 = para_d6
      endif
      if mark2 eq 'SLATB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        SLATB2 = para_d6
      endif
      if mark2 eq 'SSATB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        SSATB1 = para_d6
      endif
      if mark2 eq 'SSATB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        SSATB2 = para_d6
      endif
      if mark2 eq 'FRTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FRTB1 = para_d6
      endif
      if mark2 eq 'FRTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FRTB2 = para_d6
      endif
      if mark2 eq 'FLTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FLTB1 = para_d6
      endif
      if mark2 eq 'FLTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FLTB2 = para_d6
      endif
      if mark2 eq 'FSTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FSTB1 = para_d6
      endif
      if mark2 eq 'FSTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FSTB2 = para_d6
      endif
      if mark2 eq 'FOTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FOTB1 = para_d6
      endif
      if mark2 eq 'FOTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FOTB2 = para_d6
      endif
      if mark2 eq 'DTSMTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        DTSMTB1 = para_d6
      endif
      if mark2 eq 'DTSMTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        DTSMTB2 = para_d6
      endif
      if mark2 eq 'AMAXTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        AMAXTB1 = para_d6
      endif
      if mark2 eq 'AMAXTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        AMAXTB2 = para_d6
      endif
      if mark2 eq 'TMPFTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        TMPFTB1 = para_d6
      endif
      if mark2 eq 'TMPFTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        TMPFTB2 = para_d6
      endif
      if mark2 eq 'EEFTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        EEFTB1 = para_d6
      endif
      if mark2 eq 'EEFTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        EEFTB2 = para_d6
      endif
      if mark2 eq 'KDIFTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        KDIFTB1 = para_d6
      endif
      if mark2 eq 'KDIFTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        KDIFTB2 = para_d6
      endif
      if mark2 eq 'RFSETB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RFSETB1 = para_d6
      endif
      if mark2 eq 'RFSETB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RFSETB2 = para_d6
      endif
      if mark2 eq 'RDRSTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RDRSTB1 = para_d6
      endif
      if mark2 eq 'RDRSTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RDRSTB2 = para_d6
      endif
      if mark2 eq 'RDRRTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RDRRTB1 = para_d6
      endif
      if mark2 eq 'RDRRTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RDRRTB2 = para_d6
      endif
    endfor
    FREE_LUN,lun
 print,crop_type
  ENDIF
  
  IF crop_type EQ 2 THEN BEGIN
    ;-------------------------------------------------------------------------------------------------------------------------
    ;大豆作物参数读取界面(The specific parameter input for soybean)
    DELT = 1                       
    IWB = 1                        
    I_nutrient = 1                 
    I_stess = 2                    
    IOX = 1                        
    IAIRDU = 0                     
    i_stday = 1.0                  
    stday_start = 100              
    stday_end = 140
    DEPNR = 5                     
    TSUM1 = 720                    
    TSUM2 = 680                    
    SMFCF = 0.460                 
    SMW = 0.200                    
    SM0 = 0.570                   
    TBASEM = 5                     
    TEFFMX =30                     
    CRAIRC = 0.075                 
    RMR = 0.008                    
    RMO = 0.009
    RML = 0.008
    RMS = 0.008
    Q10 = 2.0
    CVL = 0.760                    
    CVO = 0.450
    CVR = 0.760
    CVS = 0.720
    TDWI = 120                     
    PERDL = 0.025                  
    Ju1 = 130                      
    DVSEND = 2.0                   
    SPAN = 23                     
    TBASE = 7.0                    
    RDI  = 10                     
    RRI  = 1.2                     
    SPA = 0.0                      
    RDMCR = 120                    
    RDMSOL = 200                   
    RGRLAI = 0.0100                
    ISTATE = 2                     
    LASUM = 0.0                   
    RAINT = 0.0                    
    IDEM = 115                     
    K0 = 10.789                    
    LAI_start = 0.0                
    TSUMEM = 64                    
    NOTINF = 0.0                   
    SM_t = 0.300                   
    SM = 0.300
    CFET = 1.0                     
    SPADS    =   0.050             
    SPODS    =   0.025
    SPASS    =   0.100
    SPOSS    =   0.040
    DEFLIM   =  -0.300
    TERMNL = 0
    DVSI = 0.0
    SMW = 0.099
    DVS = DVSI
    TDWI = 100
    TSUM   = 0.
    IDANTH = -99
    ;LV = FLTARR(100)
    LVAGE = FLTARR(100)
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
    SLATB1 = [0.00,0.45,0.90,2.00]                                         
    SLATB2 = [0.0014,0.0025,0.0025,0.0007]
    SSATB1 = [0.0,2.0]                                                      
    SSATB2 = [0.0,0.0]
    FRTB1 = [0.00,0.75,1.00,1.50,2.00]                                     
    FRTB2 = [0.65,0.35,0.20,0.00,0.00]
    FLTB1 = [0.00,1.00,1.15,1.30,1.50,2.00]                              
    FLTB2 = [0.70,0.65,0.65,0.48,0.25,0.15]
    FSTB1 = [0.00,1.00,1.15,1.30,1.50,2.00]                                 
    FSTB2 = [0.20,0.15,0.25,0.15,0.30,0.00]
    FOTB1 = [0.00,1.00,1.15,1.30,1.50,2.00]                                 
    FOTB2 = [0.10,0.2,0.10,0.37,0.45,0.85]
    DTSMTB1 = [0.00,7.00,30.00,45.00]                                      
    DTSMTB2 = [0.00,0.00,23.00,38.00]
    AMAXTB1 =[0.00,1.70,2.00]                                               
    AMAXTB2 =[28.00,25.00,10.00]
    TMPFTB1 = [0.00,10.00,20.00,18.00,25.00,30.00,35.00]                   
    TMPFTB2 = [0.00,0.30,0.60,0.80,1.00,1.00]
    EEFTB1 = [0,40]                                                         
    EEFTB2 = [0.40,0.40]
    KDIFTB1 = [0.0,2.0]                                                     
    KDIFTB2 = [0.8,0.8]
    RFSETB1 = [0.0,2.0]                                                     
    RFSETB2 = [1.0,1.0]
    RDRSTB1 = [0.00,1.50,1.50001,2.00]                                     
    RDRSTB2 = [0.000,0.000,0.015,0.015]
    RDRRTB1 = [0.00000,1.50000,1.50001,2.00000]                            
    RDRRTB2 = [0.000,0.000,0.020,0.020]
    
     ;-------------------------------------------------------------------------------------------------------------------------
    
;    parameter_txt_patth = 'D:\biomass\exp_data\'
    parameter_txt = parameter_txt_path + 'SOYBEAN_hongxing.txt'
    OPENR,lun,parameter_txt,/get_lun
    lines = FILE_LINES(parameter_txt)
    para_all_data = STRARR(1,Lines)
    READF,lun,para_all_data
    for i = 0 ,lines -1 do begin
    
      para = para_all_data[i]
      para1 = STRSPLIT(para,'=',/EXTRACT)
      mark1 = para1[0]
      mark2 = STRTRIM(mark1,2)
      
;         if mark2 eq 'SSI' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        SSI = FLOAT(para_data)
;      endif
;        if mark2 eq 'XDEF' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        XDEF = FLOAT(para_data)
;      endif      
;        if mark2 eq 'ZTI' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        ZTI = FLOAT(para_data)
;      endif
;        if mark2 eq 'IDRAIN' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        IDRAIN = FLOAT(para_data)
;      endif
;        if mark2 eq 'DD' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        DD = FLOAT(para_data)
;      endif
;        if mark2 eq 'SMTAB1' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        SMTAB1 = FLOAT(para_data)
;      endif
;          if mark2 eq 'SMTAB2' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        SMTAB2 = FLOAT(para_data)
;      endif    
;      if mark2 eq 'CONTAB1' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        CONTAB1 = FLOAT(para_data)
;      endif
;          if mark2 eq 'CONTAB2' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        CONTAB2 = FLOAT(para_data)
;      endif
;          if mark2 eq 'SOPE' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        SOPE = FLOAT(para_data)
;      endif
;          if mark2 eq 'KSUB' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        KSUB = FLOAT(para_data)
;      endif
      
      if mark2 eq 'DELT' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        DELT = FLOAT(para_data)
      endif
      if mark2 eq 'IWB' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        IWB = FLOAT(para_data)
      endif
      if mark2 eq 'I_nutrient' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        I_nutrient = FLOAT(para_data)
      endif
      if mark2 eq 'I_stess' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        I_stess = FLOAT(para_data)
      endif
      if mark2 eq 'IOX' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        IOX = FLOAT(para_data)
      endif
      if mark2 eq 'IAIRDU' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        IAIRDU = FLOAT(para_data)
      endif
      if mark2 eq 'i_stday' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        i_stday = FLOAT(para_data)
      endif
      if mark2 eq 'stday_start' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        stday_start = FLOAT(para_data)
      endif
      if mark2 eq 'stday_end' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        stday_end = FLOAT(para_data)
      endif
      if mark2 eq 'DEPNR' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        DEPNR = FLOAT(para_data)
      endif
      if mark2 eq 'TSUM1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TSUM1 = FLOAT(para_data)
      endif
      if mark2 eq 'TSUM2' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TSUM2 = FLOAT(para_data)
      endif
      if mark2 eq 'SMFCF' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SMFCF = FLOAT(para_data)
      endif
      if mark2 eq 'SMW' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SMW = FLOAT(para_data)
      endif
      if mark2 eq 'SM0' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SM0 = FLOAT(para_data)
      endif
      if mark2 eq 'TBASEM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TBASEM = FLOAT(para_data)
      endif
      if mark2 eq 'TEFFMX' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TEFFMX = FLOAT(para_data)
      endif
      if mark2 eq 'CRAIRC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CRAIRC = FLOAT(para_data)
      endif
      if mark2 eq 'RMR' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RMR = FLOAT(para_data)
      endif
      if mark2 eq 'RMO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RMO = FLOAT(para_data)
      endif
      if mark2 eq 'RML' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RML = FLOAT(para_data)
      endif
      if mark2 eq 'RMS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RMS = FLOAT(para_data)
      endif
      if mark2 eq 'Q10' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        Q10 = FLOAT(para_data)
      endif
      if mark2 eq 'CVL' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CVL = FLOAT(para_data)
      endif
                if mark2 eq 'CVO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CVO = FLOAT(para_data)
      endif
      if mark2 eq 'CVR' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CVR = FLOAT(para_data)
      endif
      if mark2 eq 'CVS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CVS = FLOAT(para_data)
      endif
      if mark2 eq 'TDWI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TDWI = FLOAT(para_data)
      endif
      if mark2 eq 'PERDL' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PERDL = FLOAT(para_data)
      endif
      if mark2 eq 'Ju1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        Ju1 = FLOAT(para_data)
      endif
      if mark2 eq 'DVSEND' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        DVSEND = FLOAT(para_data)
      endif
      if mark2 eq 'SPAN' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPAN = FLOAT(para_data)
      endif
      if mark2 eq 'TBASE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TBASE = FLOAT(para_data)
      endif
      if mark2 eq 'RDI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RDI = FLOAT(para_data)
      endif
      if mark2 eq 'RRI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RRI = FLOAT(para_data)
      endif
      if mark2 eq 'SPA' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPA = FLOAT(para_data)
      endif
      if mark2 eq 'RDMCR' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RDMCR = FLOAT(para_data)
      endif
      if mark2 eq 'RDMSOL' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RDMSOL = FLOAT(para_data)
      endif
      if mark2 eq 'RGRLAI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RGRLAI = FLOAT(para_data)
      endif
      if mark2 eq 'ISTATE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        ISTATE = FLOAT(para_data)
      endif
      if mark2 eq 'LASUM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        LASUM = FLOAT(para_data)
      endif
      if mark2 eq 'RAINT' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RAINT = FLOAT(para_data)
      endif
      if mark2 eq 'IDEM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        IDEM = FLOAT(para_data)
      endif
      if mark2 eq 'K0' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        K0 = FLOAT(para_data)
      endif
      if mark2 eq 'LAI_start' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        LAI_start = FLOAT(para_data)
      endif
      if mark2 eq 'TSUMEM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TSUMEM = FLOAT(para_data)
      endif
      if mark2 eq 'NOTINF' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NOTINF = FLOAT(para_data)
      endif
      if mark2 eq 'SM_t' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SM_t = FLOAT(para_data)
      endif
      if mark2 eq 'SM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SM = FLOAT(para_data)
      endif
      if mark2 eq 'CFET' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CFET = FLOAT(para_data)
      endif
      if mark2 eq 'SPADS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPADS = FLOAT(para_data)
      endif
      if mark2 eq 'SPODS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPODS = FLOAT(para_data)
      endif
      if mark2 eq 'SPASS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPASS = FLOAT(para_data)
      endif
      if mark2 eq 'SPOSS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPOSS = FLOAT(para_data)
      endif
      if mark2 eq 'DEFLIM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        DEFLIM = FLOAT(para_data)
      endif
      if mark2 eq 'TERMNL' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TERMNL = FLOAT(para_data)
      endif
      if mark2 eq 'DVSI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        DVSI = FLOAT(para_data)
      endif
      if mark2 eq 'SMW' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SMW = FLOAT(para_data)
      endif
      if mark2 eq 'TSUM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TSUM = FLOAT(para_data)
      endif
      if mark2 eq 'IDANTH' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        IDANTH = FLOAT(para_data)
      endif
      
      if mark2 eq 'SLATB1' then begin
        ;      SLATB1 = fltarr(3)
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        SLATB1 = para_d6
      endif
      if mark2 eq 'SLATB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        SLATB2 = para_d6
      endif
      if mark2 eq 'SSATB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        SSATB1 = para_d6
      endif
      if mark2 eq 'SSATB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        SSATB2 = para_d6
      endif
      if mark2 eq 'FRTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FRTB1 = para_d6
      endif
      if mark2 eq 'FRTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FRTB2 = para_d6
      endif
      if mark2 eq 'FLTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FLTB1 = para_d6
      endif
      if mark2 eq 'FLTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FLTB2 = para_d6
      endif
      if mark2 eq 'FSTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FSTB1 = para_d6
      endif
      if mark2 eq 'FSTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FSTB2 = para_d6
      endif
      if mark2 eq 'FOTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FOTB1 = para_d6
      endif
      if mark2 eq 'FOTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FOTB2 = para_d6
      endif
      if mark2 eq 'DTSMTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        DTSMTB1 = para_d6
      endif
      if mark2 eq 'DTSMTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        DTSMTB2 = para_d6
      endif
      if mark2 eq 'AMAXTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        AMAXTB1 = para_d6
      endif
      if mark2 eq 'AMAXTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        AMAXTB2 = para_d6
      endif
      if mark2 eq 'TMPFTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        TMPFTB1 = para_d6
      endif
      if mark2 eq 'TMPFTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        TMPFTB2 = para_d6
      endif
      if mark2 eq 'EEFTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        EEFTB1 = para_d6
      endif
      if mark2 eq 'EEFTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        EEFTB2 = para_d6
      endif
      if mark2 eq 'KDIFTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        KDIFTB1 = para_d6
      endif
      if mark2 eq 'KDIFTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        KDIFTB2 = para_d6
      endif
      if mark2 eq 'RFSETB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RFSETB1 = para_d6
      endif
      if mark2 eq 'RFSETB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RFSETB2 = para_d6
      endif
      if mark2 eq 'RDRSTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RDRSTB1 = para_d6
      endif
      if mark2 eq 'RDRSTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RDRSTB2 = para_d6
      endif
      if mark2 eq 'RDRRTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RDRRTB1 = para_d6
      endif
      if mark2 eq 'RDRRTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RDRRTB2 = para_d6
      endif
    endfor
    FREE_LUN,lun
    
  ENDIF
  
  IF crop_type EQ 3.0 THEN BEGIN
  
   ;-------------------------------------------------------------------------------------------------------------------------
   ;小麦作物参数读取界面(The specific parameter input for wheat)
    DELT = 1                      
    IWB = 1                       
    I_nutrient = 1                
    I_stess = 2                    
    IOX = 1                        
    IAIRDU = 0                     
    i_stday = 1.0                  
    stday_start = 110              
    stday_end = 140
    DEPNR = 4.5                   
    TSUM1 = 940                    
    TSUM2 = 650                    
    SMFCF = 0.460                  
    SMW = 0.200                    
    SM0 = 0.570                    
    TBASEM = -5                   
    TEFFMX =30                     
    CRAIRC = 0.075                 
    RMR = 0.015                    
    RMO = 0.010
    RML = 0.030
    RMS = 0.015
    Q10 = 2.0
    CVL = 0.685                    
    CVO = 0.709
    CVR = 0.694
    CVS = 0.662
    TDWI = 110                     
    PERDL = 0.030                  
    Ju1 = 121                      
    DVSEND = 2.0                   
    SPAN = 21.3                    
    TBASE = 0.0                   
    RDI  = 10                      
    RRI  = 1.2                     
    SPA = 0.0                     
    RDMCR = 125                    
    RDMSOL = 200                   
    RGRLAI = 0.00817               
    ISTATE = 2                     
    LASUM = 0.0                    
    RAINT = 0.0                    
    IDEM = 115                     
    K0 = 10.789                    
    LAI_start = 0.0                
    TSUMEM = 100                   
    NOTINF = 0.0                   
    SM_t = 0.300                   
    SM = 0.300
    CFET = 1.0                     
    SPADS    =   0.050             
    SPODS    =   0.025
    SPASS    =   0.100
    SPOSS    =   0.040
    DEFLIM   =  -0.300
    TERMNL = 0
    DVSI = 0.0
    SMW = 0.099
    DVS = DVSI
    TSUM   = 0.
    IDANTH = -99
    ;LV = FLTARR(100)
    LVAGE = FLTARR(100)
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
    SLATB1 = [0.00,0.50,2.00]                                               
    SLATB2 = [0.00212,0.00212,0.00212]
    SSATB1 = [0.0,2.0]                                                      
    SSATB2 = [0.0,0.0]
    FRTB1 = [0.00,0.10,0.20,0.35,0.40,0.50,0.70,0.90,1.20,2.00]             
    FRTB2 = [0.50,0.50,0.40,0.22,0.17,0.13,0.07,0.03,0.00,0.00]
    FLTB1 = [0.00,0.10,0.25,0.50,0.65,0.95,2.00]                            
    FLTB2 = [0.65,0.65,0.70,0.50,0.30,0.05,0.05]
    FSTB1 = [0.00,0.10,0.25,0.50,0.65,0.95,2.00]                           
    FSTB2 = [0.35,0.35,0.30,0.50,0.70,0.00,0.00]
    FOTB1 = [0.00,0.95,1.00,2.00]                                          
    FOTB2 = [0.00,0.00,0.95,0.95]
    DTSMTB1 = [0.00,30.00,45.00]                                            
    DTSMTB2 = [0.00,30.00,30.00]
    AMAXTB1 =[0.00,1.00,1.30,2.00]                                         
    AMAXTB2 =[35.83,35.83,35.83,4.48]
    TMPFTB1 = [0.00,10.00,15.00,25.00,35.00]                              
    TMPFTB2 = [0.01,0.65,1.00,1.00,0.00]
    EEFTB1 = [0,40]                                                         
    EEFTB2 = [0.45,0.45]
    KDIFTB1 = [0.0,2.0]                                                     
    KDIFTB2 = [0.6,0.6]
    RFSETB1 = [0.0,2.0]                                                     
    RFSETB2 = [1.0,1.0]
    RDRSTB1 = [0.00,1.50,1.50001,2.00]                                      
    RDRSTB2 = [0.000,0.000,0.0150,0.020]
    RDRRTB1 = [0.00000,1.50000,1.50001,2.00000]                             
    RDRRTB2 = [0.000,0.000,0.020,0.020]
     ;-------------------------------------------------------------------------------------------------------------------------
    
;    parameter_txt_patth = 'D:\biomass\exp_data\'
    parameter_txt = parameter_txt_path + 'WHEAT_hongxing.txt'
    OPENR,lun,parameter_txt,/get_lun
    lines = FILE_LINES(parameter_txt)
    para_all_data = STRARR(1,Lines)
    READF,lun,para_all_data
    for i = 0 ,lines -1 do begin
    
      para = para_all_data[i]
      para1 = STRSPLIT(para,'=',/EXTRACT)
      mark1 = para1[0]
      mark2 = STRTRIM(mark1,2)
      
      
;         if mark2 eq 'SSI' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        SSI = FLOAT(para_data)
;      endif
;        if mark2 eq 'XDEF' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        XDEF = FLOAT(para_data)
;      endif      
;        if mark2 eq 'ZTI' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        ZTI = FLOAT(para_data)
;      endif
;        if mark2 eq 'IDRAIN' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        IDRAIN = FLOAT(para_data)
;      endif
;        if mark2 eq 'DD' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        DD = FLOAT(para_data)
;      endif
;        if mark2 eq 'SMTAB1' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        SMTAB1 = FLOAT(para_data)
;      endif
;          if mark2 eq 'SMTAB2' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        SMTAB2 = FLOAT(para_data)
;      endif    
;      if mark2 eq 'CONTAB1' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        CONTAB1 = FLOAT(para_data)
;      endif
;          if mark2 eq 'CONTAB2' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        CONTAB2 = FLOAT(para_data)
;      endif
;          if mark2 eq 'SOPE' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        SOPE = FLOAT(para_data)
;      endif
;          if mark2 eq 'KSUB' then begin
;        para2 = STRSPLIT(para,'*',/EXTRACT)
;        para_data = para2[1]
;        KSUB = FLOAT(para_data)
;      endif
      if mark2 eq 'DELT' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        DELT = FLOAT(para_data)
      endif
      if mark2 eq 'IWB' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        IWB = FLOAT(para_data)
      endif
      if mark2 eq 'I_nutrient' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        I_nutrient = FLOAT(para_data)
      endif
      if mark2 eq 'I_stess' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        I_stess = FLOAT(para_data)
      endif
      if mark2 eq 'IOX' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        IOX = FLOAT(para_data)
      endif
      if mark2 eq 'IAIRDU' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        IAIRDU = FLOAT(para_data)
      endif
      if mark2 eq 'i_stday' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        i_stday = FLOAT(para_data)
      endif
      if mark2 eq 'stday_start' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        stday_start = FLOAT(para_data)
      endif
      if mark2 eq 'stday_end' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        stday_end = FLOAT(para_data)
      endif
      if mark2 eq 'DEPNR' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        DEPNR = FLOAT(para_data)
      endif
      if mark2 eq 'TSUM1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TSUM1 = FLOAT(para_data)
      endif
      if mark2 eq 'TSUM2' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TSUM2 = FLOAT(para_data)
      endif
      if mark2 eq 'SMFCF' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SMFCF = FLOAT(para_data)
      endif
      if mark2 eq 'SMW' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SMW = FLOAT(para_data)
      endif
      if mark2 eq 'SM0' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SM0 = FLOAT(para_data)
      endif
      if mark2 eq 'TBASEM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TBASEM = FLOAT(para_data)
      endif
      if mark2 eq 'TEFFMX' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TEFFMX = FLOAT(para_data)
      endif
      if mark2 eq 'CRAIRC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CRAIRC = FLOAT(para_data)
      endif
      if mark2 eq 'RMR' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RMR = FLOAT(para_data)
      endif
      if mark2 eq 'RMO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RMO = FLOAT(para_data)
      endif
      if mark2 eq 'RML' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RML = FLOAT(para_data)
      endif
      if mark2 eq 'RMS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RMS = FLOAT(para_data)
      endif
      if mark2 eq 'Q10' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        Q10 = FLOAT(para_data)
      endif
      if mark2 eq 'CVL' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CVL = FLOAT(para_data)
      endif
                if mark2 eq 'CVO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CVO = FLOAT(para_data)
      endif
      if mark2 eq 'CVR' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CVR = FLOAT(para_data)
      endif
      if mark2 eq 'CVS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CVS = FLOAT(para_data)
      endif
      if mark2 eq 'TDWI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TDWI = FLOAT(para_data)
      endif
      if mark2 eq 'PERDL' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PERDL = FLOAT(para_data)
      endif
      if mark2 eq 'Ju1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        Ju1 = FLOAT(para_data)
      endif
      if mark2 eq 'DVSEND' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        DVSEND = FLOAT(para_data)
      endif
      if mark2 eq 'SPAN' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPAN = FLOAT(para_data)
      endif
      if mark2 eq 'TBASE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TBASE = FLOAT(para_data)
      endif
      if mark2 eq 'RDI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RDI = FLOAT(para_data)
      endif
      if mark2 eq 'RRI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RRI = FLOAT(para_data)
      endif
      if mark2 eq 'SPA' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPA = FLOAT(para_data)
      endif
      if mark2 eq 'RDMCR' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RDMCR = FLOAT(para_data)
      endif
      if mark2 eq 'RDMSOL' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RDMSOL = FLOAT(para_data)
      endif
      if mark2 eq 'RGRLAI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RGRLAI = FLOAT(para_data)
      endif
      if mark2 eq 'ISTATE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        ISTATE = FLOAT(para_data)
      endif
      if mark2 eq 'LASUM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        LASUM = FLOAT(para_data)
      endif
      if mark2 eq 'RAINT' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        RAINT = FLOAT(para_data)
      endif
      if mark2 eq 'IDEM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        IDEM = FLOAT(para_data)
      endif
      if mark2 eq 'K0' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        K0 = FLOAT(para_data)
      endif
      if mark2 eq 'LAI_start' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        LAI_start = FLOAT(para_data)
      endif
      if mark2 eq 'TSUMEM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TSUMEM = FLOAT(para_data)
      endif
      if mark2 eq 'NOTINF' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NOTINF = FLOAT(para_data)
      endif
      if mark2 eq 'SM_t' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SM_t = FLOAT(para_data)
      endif
      if mark2 eq 'SM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SM = FLOAT(para_data)
      endif
      if mark2 eq 'CFET' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        CFET = FLOAT(para_data)
      endif
      if mark2 eq 'SPADS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPADS = FLOAT(para_data)
      endif
      if mark2 eq 'SPODS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPODS = FLOAT(para_data)
      endif
      if mark2 eq 'SPASS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPASS = FLOAT(para_data)
      endif
      if mark2 eq 'SPOSS' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SPOSS = FLOAT(para_data)
      endif
      if mark2 eq 'DEFLIM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        DEFLIM = FLOAT(para_data)
      endif
      if mark2 eq 'TERMNL' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TERMNL = FLOAT(para_data)
      endif
      if mark2 eq 'DVSI' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        DVSI = FLOAT(para_data)
      endif
      if mark2 eq 'SMW' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        SMW = FLOAT(para_data)
      endif
      if mark2 eq 'TSUM' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        TSUM = FLOAT(para_data)
      endif
      if mark2 eq 'IDANTH' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        IDANTH = FLOAT(para_data)
      endif
      
      if mark2 eq 'SLATB1' then begin
        ;      SLATB1 = fltarr(3)
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        SLATB1 = para_d6
      endif
      if mark2 eq 'SLATB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        SLATB2 = para_d6
      endif
      if mark2 eq 'SSATB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        SSATB1 = para_d6
      endif
      if mark2 eq 'SSATB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        SSATB2 = para_d6
      endif
      if mark2 eq 'FRTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FRTB1 = para_d6
      endif
      if mark2 eq 'FRTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FRTB2 = para_d6
      endif
      if mark2 eq 'FLTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FLTB1 = para_d6
      endif
      if mark2 eq 'FLTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FLTB2 = para_d6
      endif
      if mark2 eq 'FSTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FSTB1 = para_d6
      endif
      if mark2 eq 'FSTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FSTB2 = para_d6
      endif
      if mark2 eq 'FOTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FOTB1 = para_d6
      endif
      if mark2 eq 'FOTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        FOTB2 = para_d6
      endif
      if mark2 eq 'DTSMTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        DTSMTB1 = para_d6
      endif
      if mark2 eq 'DTSMTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        DTSMTB2 = para_d6
      endif
      if mark2 eq 'AMAXTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        AMAXTB1 = para_d6
      endif
      if mark2 eq 'AMAXTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        AMAXTB2 = para_d6
      endif
      if mark2 eq 'TMPFTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        TMPFTB1 = para_d6
      endif
      if mark2 eq 'TMPFTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        TMPFTB2 = para_d6
      endif
      if mark2 eq 'EEFTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        EEFTB1 = para_d6
      endif
      if mark2 eq 'EEFTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        EEFTB2 = para_d6
      endif
      if mark2 eq 'KDIFTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        KDIFTB1 = para_d6
      endif
      if mark2 eq 'KDIFTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        KDIFTB2 = para_d6
      endif
      if mark2 eq 'RFSETB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RFSETB1 = para_d6
      endif
      if mark2 eq 'RFSETB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RFSETB2 = para_d6
      endif
      if mark2 eq 'RDRSTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RDRSTB1 = para_d6
      endif
      if mark2 eq 'RDRSTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RDRSTB2 = para_d6
      endif
      if mark2 eq 'RDRRTB1' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RDRRTB1 = para_d6
      endif
      if mark2 eq 'RDRRTB2' then begin
        para_d1 = STRSPLIT(para,'[',/EXTRACT)
        para_d2 = para_d1[1]
        para_d3 = STRSPLIT(para_d2,']',/EXTRACT)
        para_d4 = para_d3[0]
        para_d5 = STRSPLIT(para_d4,',',/EXTRACT)
        para_d6 = FLOAT(para_d5)
        RDRRTB2 = para_d6
      endif
    endfor
    FREE_LUN,lun
  ENDIF
  IF crop_type EQ 0 THEN BEGIN
     ;-------------------------------------------------------------------------------------------------------------------------
   ;其它作物参数读取界面(The specific parameter input for other crop)
    DELT = 1                       
    IWB = 1                        
    I_nutrient = 1                 
    I_stess = 2                    
    IOX = 1                        
    IAIRDU = 0                     
    i_stday = 1.0                  
    stday_start = 100              
    stday_end = 140
    DEPNR = 5                     
    TSUM1 = 720                    
    TSUM2 = 680                    
    SMFCF = 0.460                  
    SMW = 0.200                    
    SM0 = 0.570                    
    TBASEM = 5                     
    TEFFMX =30                     
    CRAIRC = 0.075                 
    RMR = 0.008                    
    RMO = 0.009
    RML = 0.008
    RMS = 0.008
    Q10 = 2.0
    CVL = 0.760                    
    CVO = 0.450
    CVR = 0.760
    CVS = 0.720
    TDWI = 120                     
    PERDL = 0.025                  
    Ju1 = 135                      
    DVSEND = 2.0                  
    SPAN = 23                      
    TBASE = 7.0                    
    RDI  = 10                      
    RRI  = 1.2                     
    SPA = 0.0                      
    RDMCR = 120                    
    RDMSOL = 200                   
    RGRLAI = 0.0100                
    ISTATE = 2                     
    LASUM = 0.0                    
    RAINT = 0.0                   
    IDEM = 115                    
    K0 = 10.789                    
    LAI_start = 0.0                
    TSUMEM = 64                    
    NOTINF = 0.0                   
    SM_t = 0.300                   
    SM = 0.300
    CFET = 1.0                     
    SPADS    =   0.050             
    SPODS    =   0.025
    SPASS    =   0.100
    SPOSS    =   0.040
    DEFLIM   =  -0.300
    TERMNL = 0
    DVSI = 0.0
    SMW = 0.099
    DVS = DVSI
    TDWI = 100
    TSUM   = 0.
    IDANTH = -99
    ;LV = FLTARR(100)
    LVAGE = FLTARR(100)
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
    SLATB1 = [0.00,0.45,0.90,2.00]                                         
    SLATB2 = [0.0014,0.0025,0.0025,0.0007]
    SSATB1 = [0.0,2.0]                                                      
    SSATB2 = [0.0,0.0]
    FRTB1 = [0.00,0.75,1.00,1.50,2.00]                                      
    FRTB2 = [0.65,0.35,0.20,0.00,0.00]
    FLTB1 = [0.00,1.00,1.15,1.30,1.50,2.00]                                
    FLTB2 = [0.70,0.65,0.65,0.48,0.25,0.15]
    FSTB1 = [0.00,1.00,1.15,1.30,1.50,2.00]                                 
    FSTB2 = [0.20,0.15,0.25,0.15,0.30,0.00]
    FOTB1 = [0.00,1.00,1.15,1.30,1.50,2.00]                                
    FOTB2 = [0.10,0.2,0.10,0.37,0.45,0.85]
    DTSMTB1 = [0.00,7.00,30.00,45.00]                                       
    DTSMTB2 = [0.00,0.00,23.00,38.00]
    AMAXTB1 =[0.00,1.70,2.00]                                               
    AMAXTB2 =[28.00,25.00,10.00]
    TMPFTB1 = [0.00,10.00,20.00,18.00,25.00,30.00,35.00]                    
    TMPFTB2 = [0.00,0.30,0.60,0.80,1.00,1.00]
    EEFTB1 = [0,40]                                                         
    EEFTB2 = [0.40,0.40]
    KDIFTB1 = [0.0,2.0]                                                     
    KDIFTB2 = [0.8,0.8]
    RFSETB1 = [0.0,2.0]                                                     
    RFSETB2 = [1.0,1.0]
    RDRSTB1 = [0.00,1.50,1.50001,2.00]                                      
    RDRSTB2 = [0.000,0.000,0.015,0.015]
    RDRRTB1 = [0.00000,1.50000,1.50001,2.00000]                             
    RDRRTB2 = [0.000,0.000,0.020,0.020]
  ENDIF
END