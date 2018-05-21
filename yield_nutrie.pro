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
;计算养分限制产量并返回主程序(Calculate the nutrient-limited yield and return to the main project
;corresponds to theNUTRIE.FOR module of orginal WOFOST in fortran)

FUNCTION CAL_NUTRIE,DUROPT,parameter_txt_path,WSO_DATA,WST_DATA,WLV_data,ns,nl,crop_type,TWST_DATA1,TWLV_DATA1,TWSO_DATA1,TWRT_DATA1,RD
  COMPILE_OPT idl2
  ;养分相关参数输入 (Soil nutrient parameters input)
  fertilizer_factor = 1.0                         ;是否考虑施肥的影响(Whether the fertilizer is consindered)
  H_soil = 1.0                                    ;土壤层最大深度(The maximum depth of black soil)
  H_ROOT = RD                                     ;根系的实际深度(The depth of root)
  k_nso = 0.75
  k_pso = 0.75
  k_kso = 0.75
  N_upt_total = 0.0
  P_upt_total = 0.0
  K_upt_total = 0.0
  EYNP = 0.0
  EYPN = 0.0
  EYNK = 0.0
  EYKN = 0.0
  EYPK = 0.0
  EYKP = 0.0
  PBASE1    =  40.00
  NBASE1    = 316
  KBASE1    = 176
  IF crop_type EQ 1 THEN BEGIN
    ;----------------------------------------------------------------------------------------------------------------------------------
    ;玉米养分参数读取界面(The specific parameter input for spring maize)
    NFIX = 0.00          ;生物固氮百分比(The percentage of biological fixation of nitrogen)
    NMINSO   =   0.0095  ;存储器官最大最小的N、P、K的浓度(The max-min concentration of N, P, K in storage organs )
    NMAXSO   =   0.0220
    PMINSO   =   0.0017
    PMAXSO   =   0.0075
    KMINSO   =   0.0020
    KMAXSO   =   0.0060
    NMINVE   =   0.0040  ;营养器官最大最小的N、P、K的浓度(The max-min concentration of N, P, K in  vegetative organs)
    NMAXVE   =   0.0125
    PMINVE   =   0.0004
    PMAXVE   =   0.0030
    KMINVE   =   0.0050
    KMAXVE   =   0.0200
    YZERO    =   400.0   ;产量为0时营养器官的最大重量(The maximum biomass of vegetative organs when the yield is zero)
    NBASE    =   95      ;基础N、P、K含量及其施肥供给量(The base soil nutrient content)
    NBASE    =   95.00
    NREC     =   0.50
    PBASE    =   35.00
    PREC     =   0.50
    KBASE    =   95.00
    KREC     =   0.50
    KF       =   150.5    ;施肥量(Fertilization)
    NF       =   261.5
    PF       =   138.0
    n_lv     =   0.0
    k_lv     =   0.0
    P_lv     =   0.0
    k_nso = 0.78
    k_pso = 0.78
    k_kso = 0.78
    ;------------------------------------------------------------------------------------------------------------------------------
    ;作物吸收相关参数（Crop uptake soil nutrient）
    day_k_crop = MAX([1,DUROPT])
    C_farm = 87273
    k_N_a = 0.2778
    k_N_c = -79.5
    k_P_a = 0.0679
    k_P_c = -87.6
    k_K_a = 0.2834
    k_K_c = -71.3
    c_m_N = 1.080
    c_m_P = 1.445
    c_m_K = 0.399
    ;-----------------------------------------------------------------------------------------------------------------------------
  
    
    parameter_txt = parameter_txt_path + 'MAIZE_soilnutrie_hongxing.txt'
    OPENR,lun,parameter_txt,/get_lun
    lines = FILE_LINES(parameter_txt)
    para_all_data = STRARR(1,Lines)
    READF,lun,para_all_data
    
    for i = 0 ,lines -1 do begin
    
      para = para_all_data[i]
      para1 = STRSPLIT(para,'=',/EXTRACT)
      mark1 = para1[0]
      mark2 = STRTRIM(mark1,2)
      if mark2 eq 'NFIX' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NFIX = FLOAT(para_data)
      endif
      if mark2 eq 'NMINSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NMINSO = FLOAT(para_data)
      endif
      if mark2 eq 'NMAXSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NMAXSO = FLOAT(para_data)
      endif
      if mark2 eq 'PMINSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PMINSO = FLOAT(para_data)
      endif
      if mark2 eq 'PMAXSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PMAXSO = FLOAT(para_data)
      endif
      if mark2 eq 'KMINSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KMINSO = FLOAT(para_data)
      endif
      if mark2 eq 'KMAXSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KMAXSO = FLOAT(para_data)
      endif
      if mark2 eq 'NMINVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NMINVE = FLOAT(para_data)
      endif
      if mark2 eq 'NMAXVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NMAXVE = FLOAT(para_data)
      endif
      if mark2 eq 'PMINVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PMINVE = FLOAT(para_data)
      endif
      if mark2 eq 'PMAXVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PMAXVE = FLOAT(para_data)
      endif
      if mark2 eq 'KMINVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KMINVE = FLOAT(para_data)
      endif
      if mark2 eq 'KMAXVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KMAXVE = FLOAT(para_data)
      endif
      if mark2 eq 'YZERO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        YZERO = FLOAT(para_data)
      endif
      if mark2 eq 'NBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'NREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NREC = FLOAT(para_data)
      endif
      if mark2 eq 'YZERO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        YZERO = FLOAT(para_data)
      endif
      if mark2 eq 'NBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'NREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NREC = FLOAT(para_data)
      endif
      if mark2 eq 'YZERO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        YZERO = FLOAT(para_data)
      endif
      if mark2 eq 'NBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'NREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NREC = FLOAT(para_data)
      endif
      if mark2 eq 'YZERO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        YZERO = FLOAT(para_data)
      endif
      if mark2 eq 'NBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'NREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NREC = FLOAT(para_data)
      endif
      if mark2 eq 'PBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'PREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PREC = FLOAT(para_data)
      endif
      if mark2 eq 'KBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'KREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KREC = FLOAT(para_data)
      endif
      if mark2 eq 'KF' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KF = FLOAT(para_data)
      endif
      if mark2 eq 'NF' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NF = FLOAT(para_data)
      endif
      if mark2 eq 'PF' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PF = FLOAT(para_data)
      endif
      if mark2 eq 'n_lv' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        n_lv = FLOAT(para_data)
      endif
      if mark2 eq 'p_lv' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        p_lv = FLOAT(para_data)
      endif
      if mark2 eq 'k_lv' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_lv = FLOAT(para_data)
      endif
      if mark2 eq 'k_nso' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_nso = FLOAT(para_data)
      endif
      if mark2 eq 'k_pso' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_pso = FLOAT(para_data)
      endif
      if mark2 eq 'k_kso' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_kso = FLOAT(para_data)
      endif
      if mark2 eq 'C_farm' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        C_farm = FLOAT(para_data)
      endif
      if mark2 eq 'k_N_a' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_N_a = FLOAT(para_data)
      endif
      if mark2 eq 'k_N_c' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_N_c = FLOAT(para_data)
      endif
      if mark2 eq 'k_P_a' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_P_a = FLOAT(para_data)
      endif
      if mark2 eq 'k_P_c' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_P_c = FLOAT(para_data)
      endif
      if mark2 eq 'k_K_a' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_K_a = FLOAT(para_data)
      endif
      if mark2 eq 'k_K_c' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_K_c = FLOAT(para_data)
      endif
      if mark2 eq 'c_m_N' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        c_m_N = FLOAT(para_data)
      endif
      if mark2 eq 'c_m_P' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        c_m_P = FLOAT(para_data)
      endif
      if mark2 eq 'c_m_K' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        c_m_K = FLOAT(para_data)
      endif
      if mark2 eq 'fertilizer_factor' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        fertilizer_factor = FLOAT(para_data)
      endif
      if mark2 eq 'H_soil' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        H_soil = FLOAT(para_data)
      endif
    endfor
    print,k_K_a
    for i_nu_upt = 1, day_k_crop do begin
      N_upt_day = k_N_a/i_nu_upt^2 *EXP(k_N_c/i_nu_upt)
      N_upt_total = N_upt_total + N_upt_day
      P_upt_day = k_P_a/i_nu_upt^2 *EXP(k_P_c/i_nu_upt)
      P_upt_total = P_upt_total + P_upt_day
      K_upt_day = k_K_a/i_nu_upt^2 *EXP(k_K_c/i_nu_upt)
      K_upt_total = K_upt_total + K_upt_day
    endfor
    UPT_CROP_N = N_upt_total/1955*C_farm*c_m_N
    UPT_CROP_P = P_upt_total/358*C_farm*c_m_P
    UPT_CROP_K = K_upt_total/1046.5*C_farm*c_m_K
  ENDIF
  IF crop_type EQ 2 THEN BEGIN
    ;----------------------------------------------------------------------------------------------------------------------------------
    ;大豆养分参数读取界面(The specific parameter input for soybean)
    NFIX = 0.0
    NMINSO   =   0.0350  
    NMAXSO   =   0.0560
    PMINSO   =   0.0027
    PMAXSO   =   0.0080
    KMINSO   =   0.0120
    KMAXSO   =   0.0260
    NMINVE   =   0.0070  
    NMAXVE   =   0.0220
    PMINVE   =   0.0011
    PMAXVE   =   0.0052
    KMINVE   =   0.0070
    KMAXVE   =   0.0240
    YZERO    =   400.0    
    NBASE    =   95       
    NBASE    =   95.00
    NREC     =   0.50
    PBASE    =  19.00
    PREC     =   0.50
    KBASE    =   95.00
    KREC     =   0.50
    KF       =   163
    NF       =   121
    PF       =   75
    n_lv     =   0.0
    k_lv     =   0.0
    P_lv     =   0.0
    k_nso = 0.85
    k_pso = 0.75
    k_kso = 0.75
    
    ;------------------------------------------------------------------------------------------------------------------------------
    
    day_k_crop = MAX([1,DUROPT])
    
    C_farm = 345454
    k_N_a = 0.0587
    k_N_c = -69.8
    k_P_a = 0.0073
    k_P_c = -75.6
    k_K_a = 0.0744
    k_K_c = -62.5
    c_m_N = 0.850
    c_m_P = 1.077
    c_m_K = 0.314
    ;-----------------------------------------------------------------------------------------------------------------------------
    ;锟斤拷锟斤拷锟饺??
    
    parameter_txt = parameter_txt_path + 'SOYBEAN_soilnutrie_hongxing.txt'
    OPENR,lun,parameter_txt,/get_lun
    lines = FILE_LINES(parameter_txt)
    para_all_data = STRARR(1,Lines)
    READF,lun,para_all_data
    
    for i = 0 ,lines -1 do begin
    
      para = para_all_data[i]
      para1 = STRSPLIT(para,'=',/EXTRACT)
      mark1 = para1[0]
      mark2 = STRTRIM(mark1,2)
      if mark2 eq 'NFIX' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NFIX = FLOAT(para_data)
      endif
      if mark2 eq 'NMINSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NMINSO = FLOAT(para_data)
      endif
      if mark2 eq 'NMAXSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NMAXSO = FLOAT(para_data)
      endif
      if mark2 eq 'PMINSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PMINSO = FLOAT(para_data)
      endif
      if mark2 eq 'PMAXSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PMAXSO = FLOAT(para_data)
      endif
      if mark2 eq 'KMINSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KMINSO = FLOAT(para_data)
      endif
      if mark2 eq 'KMAXSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KMAXSO = FLOAT(para_data)
      endif
      if mark2 eq 'NMINVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NMINVE = FLOAT(para_data)
      endif
      if mark2 eq 'NMAXVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NMAXVE = FLOAT(para_data)
      endif
      if mark2 eq 'PMINVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PMINVE = FLOAT(para_data)
      endif
      if mark2 eq 'PMAXVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PMAXVE = FLOAT(para_data)
      endif
      if mark2 eq 'KMINVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KMINVE = FLOAT(para_data)
      endif
      if mark2 eq 'KMAXVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KMAXVE = FLOAT(para_data)
      endif
      if mark2 eq 'YZERO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        YZERO = FLOAT(para_data)
      endif
      if mark2 eq 'NBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'NREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NREC = FLOAT(para_data)
      endif
      if mark2 eq 'YZERO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        YZERO = FLOAT(para_data)
      endif
      if mark2 eq 'NBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'NREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NREC = FLOAT(para_data)
      endif
      if mark2 eq 'YZERO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        YZERO = FLOAT(para_data)
      endif
      if mark2 eq 'NBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'NREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NREC = FLOAT(para_data)
      endif
      if mark2 eq 'YZERO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        YZERO = FLOAT(para_data)
      endif
      if mark2 eq 'NBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'NREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NREC = FLOAT(para_data)
      endif
      if mark2 eq 'PBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'PREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PREC = FLOAT(para_data)
      endif
      if mark2 eq 'KBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'KREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KREC = FLOAT(para_data)
      endif
      if mark2 eq 'KF' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KF = FLOAT(para_data)
      endif
      if mark2 eq 'NF' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NF = FLOAT(para_data)
      endif
      if mark2 eq 'PF' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PF = FLOAT(para_data)
      endif
      if mark2 eq 'n_lv' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        n_lv = FLOAT(para_data)
      endif
      if mark2 eq 'p_lv' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        p_lv = FLOAT(para_data)
      endif
      if mark2 eq 'k_lv' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_lv = FLOAT(para_data)
      endif
      if mark2 eq 'k_nso' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_nso = FLOAT(para_data)
      endif
      if mark2 eq 'k_pso' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_pso = FLOAT(para_data)
      endif
      if mark2 eq 'k_kso' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_kso = FLOAT(para_data)
      endif
      if mark2 eq 'C_farm' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        C_farm = FLOAT(para_data)
      endif
      if mark2 eq 'k_N_a' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_N_a = FLOAT(para_data)
      endif
      if mark2 eq 'k_N_c' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_N_c = FLOAT(para_data)
      endif
      if mark2 eq 'k_P_a' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_P_a = FLOAT(para_data)
      endif
      if mark2 eq 'k_P_c' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_P_c = FLOAT(para_data)
      endif
      if mark2 eq 'k_K_a' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_K_a = FLOAT(para_data)
      endif
      if mark2 eq 'k_K_c' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_K_c = FLOAT(para_data)
      endif
      if mark2 eq 'c_m_N' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        c_m_N = FLOAT(para_data)
      endif
      if mark2 eq 'c_m_P' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        c_m_P = FLOAT(para_data)
      endif
      if mark2 eq 'c_m_K' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        c_m_K = FLOAT(para_data)
      endif
      if mark2 eq 'fertilizer_factor' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        fertilizer_factor = FLOAT(para_data)
      endif
      if mark2 eq 'H_soil' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        H_soil = FLOAT(para_data)
      endif
    endfor
    
    for i_nu_upt = 1, day_k_crop do begin
      N_upt_day = k_N_a/i_nu_upt^2 *EXP(k_N_c/i_nu_upt)
      N_upt_total = N_upt_total + N_upt_day
      P_upt_day = k_P_a/i_nu_upt^2 *EXP(k_P_c/i_nu_upt)
      P_upt_total = P_upt_total + P_upt_day
      K_upt_day = k_K_a/i_nu_upt^2 *EXP(k_K_c/i_nu_upt)
      K_upt_total = K_upt_total + K_upt_day
    endfor
    UPT_CROP_N = N_upt_total/1815*C_farm*c_m_N
    UPT_CROP_P = P_upt_total/323*C_farm*c_m_P
    UPT_CROP_K = K_upt_total/971.5*C_farm*c_m_K
  ENDIF
  IF crop_type EQ 3 OR crop_type EQ 0 THEN BEGIN
    ;----------------------------------------------------------------------------------------------------------------------------------
    ;小麦养分参数读取界面(The specific parameter input for wheat)
    NFIX = 0.0
    NMINSO   =   0.0110  
    NMAXSO   =   0.0310
    PMINSO   =   0.0016
    PMAXSO   =   0.0060
    KMINSO   =   0.0030
    KMAXSO   =   0.0080
    NMINVE   =   0.0030  
    NMAXVE   =   0.0105
    PMINVE   =   0.0004
    PMAXVE   =   0.0020
    KMINVE   =   0.0070
    KMAXVE   =   0.0280
    YZERO    =   400.0    
    NBASE    =   95       
    NBASE    =   95.00
    NREC     =   0.50
    PBASE    =  19.00
    PREC     =   0.50
    KBASE    =   95.00
    KREC     =   0.50
    KF       =   163
    NF       =   121
    PF       =   75
    n_lv     =   0.0
    k_lv     =   0.0
    P_lv     =   0.0
    k_nso = 0.85
    k_pso = 0.75
    k_kso = 0.75
    ;------------------------------------------------------------------------------------------------------------------------------
    ;作物养分吸收参数赋值
    day_k_crop = MAX([1,DUROPT])
    C_farm = 345454
    k_N_a = 0.0587
    k_N_c = -69.8
    k_P_a = 0.0073
    k_P_c = -75.6
    k_K_a = 0.0744
    k_K_c = -62.5
    c_m_N = 0.850
    c_m_P = 1.077
    c_m_K = 0.314
    
    ;-----------------------------------------------------------------------------------------------------------------------------

    
    parameter_txt = parameter_txt_path + 'WHEAT_soilnutrie_hongxing.txt'
    OPENR,lun,parameter_txt,/get_lun
    lines = FILE_LINES(parameter_txt)
    para_all_data = STRARR(1,Lines)
    READF,lun,para_all_data
    
    for i = 0 ,lines -1 do begin
    
      para = para_all_data[i]
      para1 = STRSPLIT(para,'=',/EXTRACT)
      mark1 = para1[0]
      mark2 = STRTRIM(mark1,2)
      if mark2 eq 'NFIX' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NFIX = FLOAT(para_data)
      endif
      if mark2 eq 'NMINSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NMINSO = FLOAT(para_data)
      endif
      if mark2 eq 'NMAXSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NMAXSO = FLOAT(para_data)
      endif
      if mark2 eq 'PMINSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PMINSO = FLOAT(para_data)
      endif
      if mark2 eq 'PMAXSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PMAXSO = FLOAT(para_data)
      endif
      if mark2 eq 'KMINSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KMINSO = FLOAT(para_data)
      endif
      if mark2 eq 'KMAXSO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KMAXSO = FLOAT(para_data)
      endif
      if mark2 eq 'NMINVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NMINVE = FLOAT(para_data)
      endif
      if mark2 eq 'NMAXVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NMAXVE = FLOAT(para_data)
      endif
      if mark2 eq 'PMINVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PMINVE = FLOAT(para_data)
      endif
      if mark2 eq 'PMAXVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PMAXVE = FLOAT(para_data)
      endif
      if mark2 eq 'KMINVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KMINVE = FLOAT(para_data)
      endif
      if mark2 eq 'KMAXVE' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KMAXVE = FLOAT(para_data)
      endif
      if mark2 eq 'YZERO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        YZERO = FLOAT(para_data)
      endif
      if mark2 eq 'NBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'NREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NREC = FLOAT(para_data)
      endif
      if mark2 eq 'YZERO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        YZERO = FLOAT(para_data)
      endif
      if mark2 eq 'NBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'NREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NREC = FLOAT(para_data)
      endif
      if mark2 eq 'YZERO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        YZERO = FLOAT(para_data)
      endif
      if mark2 eq 'NBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'NREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NREC = FLOAT(para_data)
      endif
      if mark2 eq 'YZERO' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        YZERO = FLOAT(para_data)
      endif
      if mark2 eq 'NBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'NREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NREC = FLOAT(para_data)
      endif
      if mark2 eq 'PBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'PREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PREC = FLOAT(para_data)
      endif
      if mark2 eq 'KBASE1' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KBASE1 = FLOAT(para_data)
      endif
      if mark2 eq 'KREC' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KREC = FLOAT(para_data)
      endif
      if mark2 eq 'KF' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        KF = FLOAT(para_data)
      endif
      if mark2 eq 'NF' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        NF = FLOAT(para_data)
      endif
      if mark2 eq 'PF' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        PF = FLOAT(para_data)
      endif
      if mark2 eq 'n_lv' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        n_lv = FLOAT(para_data)
      endif
      if mark2 eq 'p_lv' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        p_lv = FLOAT(para_data)
      endif
      if mark2 eq 'k_lv' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_lv = FLOAT(para_data)
      endif
      if mark2 eq 'k_nso' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_nso = FLOAT(para_data)
      endif
      if mark2 eq 'k_pso' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_pso = FLOAT(para_data)
      endif
      if mark2 eq 'k_kso' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_kso = FLOAT(para_data)
      endif
      if mark2 eq 'C_farm' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        C_farm = FLOAT(para_data)
      endif
      if mark2 eq 'k_N_a' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_N_a = FLOAT(para_data)
      endif
      if mark2 eq 'k_N_c' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_N_c = FLOAT(para_data)
      endif
      if mark2 eq 'k_P_a' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_P_a = FLOAT(para_data)
      endif
      if mark2 eq 'k_P_c' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_P_c = FLOAT(para_data)
      endif
      if mark2 eq 'k_K_a' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_K_a = FLOAT(para_data)
      endif
      if mark2 eq 'k_K_c' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        k_K_c = FLOAT(para_data)
      endif
      if mark2 eq 'c_m_N' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        c_m_N = FLOAT(para_data)
      endif
      if mark2 eq 'c_m_P' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        c_m_P = FLOAT(para_data)
      endif
      if mark2 eq 'c_m_K' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        c_m_K = FLOAT(para_data)
      endif
      if mark2 eq 'fertilizer_factor' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        fertilizer_factor = FLOAT(para_data)
      endif
      if mark2 eq 'H_soil' then begin
        para2 = STRSPLIT(para,'*',/EXTRACT)
        para_data = para2[1]
        H_soil = FLOAT(para_data)
      endif
    endfor
    
    for i_nu_upt = 1, day_k_crop do begin
      N_upt_day = k_N_a/i_nu_upt^2 *EXP(k_N_c/i_nu_upt)
      N_upt_total = N_upt_total + N_upt_day
      P_upt_day = k_P_a/i_nu_upt^2 *EXP(k_P_c/i_nu_upt)
      P_upt_total = P_upt_total + P_upt_day
      K_upt_day = k_K_a/i_nu_upt^2 *EXP(k_K_c/i_nu_upt)
      K_upt_total = K_upt_total + K_upt_day
    endfor
    UPT_CROP_N = N_upt_total/1815*C_farm*c_m_N
    UPT_CROP_P = P_upt_total/323*C_farm*c_m_P
    UPT_CROP_K = K_upt_total/971.5*C_farm*c_m_K
  ENDIF
  IF crop_type EQ 0 THEN NFIX = 0.00
  NBASE = PBASE*7.90
  KBASE = PBASE*4.40
  IF fertilizer_factor EQ 1.0 THEN BEGIN
    PBASE  = (PBASE1 *0.5*MIN([H_soil,H_ROOT])*11 )*UPT_CROP_P
    NBASE  = (NBASE1 *0.5*MIN([H_soil,H_ROOT])*11 )*UPT_CROP_N
    KBASE  = (KBASE1 *0.5*MIN([H_soil,H_ROOT])*11 )*UPT_CROP_K
  ENDIF
  
  ;---------------------------------------------------------------------------------------------

  IF fertilizer_factor NE 1.0 THEN BEGIN
    NBASE = PBASE*7.90
    KBASE = PBASE*4.40
  ENDIF
  
  NUTRIE_data = FLTARR(ns,nl)
  NUTRIE_data1 = FLTARR(ns,nl)
  
  UPTB1 = [0.0,40.0,80.0,120.0,240.0,360.0,1000.0]
  UPTB2 = [0.0,0.4,0.7,1.0,1.6,2.0,2.0]
  AF1 = UPTB1
  AF2 = UPTB2
  AF3 = DUROPT
  NUTRIE_1  = CAL_AFGEN(AF1,AF2,AF3)
  NBAS = NBASE*NUTRIE_1 + n_lv + NF *UPT_CROP_N
  PBAS = PBASE*NUTRIE_1 + P_lv + PF *UPT_CROP_P
  KBAS = KBASE*NUTRIE_1 + K_lv + KF *UPT_CROP_K
  NZERO = YZERO*(1.-NFIX)*(NMAXVE+2.*NMINVE)/3.
  PZERO = YZERO*(PMAXVE+2.*PMINVE)/3.
  KZERO = YZERO*(KMAXVE+2.*KMINVE)/3.
  NBALVE = (NMAXVE+NMINVE)/2.
  NBALSO = (NMAXSO+NMINSO)/2.
  PBALVE = (PMAXVE+PMINVE)/2.
  PBALSO = (PMAXSO+PMINSO)/2.
  KBALVE = (KMAXVE+KMINVE)/2.
  KBALSO = (KMAXSO+KMINSO)/2.
  
  
  FOR i_ns = 0,ns-1 DO BEGIN
    FOR i_nl = 0,nl-1 DO BEGIN
    
      TWLVW = TWLV_DATA1[i_ns,i_nl]
      TWSTW = TWST_DATA1[i_ns,i_nl]
      TWSOW = TWST_DATA1[i_ns,i_nl] + TWRT_DATA1[i_ns,i_nl]
      SOOPT = WSO_DATA[i_ns,i_nl]
      STOPT = WST_DATA[i_ns,i_nl]
      LVOPT = WLV_data[i_ns,i_nl]
      IF SOOPT LT 50 THEN CONTINUE
      
      NREQO  = (NBALVE*(LVOPT+STOPT)+NBALSO*SOOPT *k_nso)*(1.-NFIX)
      NREQW  = (NBALVE*(TWLVW+TWSTW)+NBALSO*TWSOW *k_nso)*(1.-NFIX)
      PREQO  = PBALVE*(LVOPT+STOPT)+PBALSO*SOOPT*k_pso
      PREQW  = PBALVE*(TWLVW+TWSTW)+PBALSO*TWSOW*k_pso
      KREQO  = KBALVE*(LVOPT+STOPT)+KBALSO*SOOPT*k_kso
      KREQW  = KBALVE*(TWLVW+TWSTW)+KBALSO*TWSOW*k_kso
      NREQA  = (NMAXVE*(LVOPT+STOPT)+NMAXSO*SOOPT*k_nso)*(1.-NFIX)
      NREQD  = (NMINVE*(LVOPT+STOPT)+NMINSO*SOOPT*k_nso)*(1.-NFIX)
      PREQA  = PMAXVE*(LVOPT+STOPT)+PMAXSO*SOOPT*k_pso
      PREQD  = PMINVE*(LVOPT+STOPT)+PMINSO*SOOPT*k_pso
      KREQA  = KMAXVE*(LVOPT+STOPT)+KMAXSO*SOOPT*k_kso
      KREQD  = KMINVE*(LVOPT+STOPT)+KMINSO*SOOPT*k_kso
      NFERTO = MAX([0.,(NREQO-NBAS)/NREC])
      NFERTW = MAX([0.,(NREQW-NBAS)/NREC])
      PFERTO = MAX([0.,(PREQO-PBAS)/PREC])
      PFERTW = MAX([0.,(PREQW-PBAS)/PREC])
      KFERTO = MAX([0.,(KREQO-KBAS)/KREC])
      KFERTW = MAX([0.,(KREQW-KBAS)/KREC])
      IF SOOPT GT 200 THEN BEGIN
        YLDO   = SOOPT
        IORGAN = 3
      ENDIF ELSE BEGIN
        IF STOPT LE 200 THEN BEGIN
          YLDO   = MAX([5.,LVOPT])
          IORGAN = 1
        ENDIF ELSE BEGIN
          YLDO   = STOPT
          IORGAN = 2
        ENDELSE
      ENDELSE
      IF IORGAN NE 3 THEN BEGIN
        NZERO  = 0.
        PZERO  = 0.
        KZERO  = 0.
      ENDIF
      YNRATD = YLDO/MAX([0.1,NREQD-NZERO])                        
      YNRATA = YLDO/MAX([0.1,NREQA-NZERO])
      YPRATD = YLDO/MAX([0.1,PREQD-PZERO])
      YPRATA = YLDO/MAX([0.1,PREQA-PZERO])
      YKRATD = YLDO/MAX([0.1,KREQD-KZERO])
      YKRATA = YLDO/MAX([0.1,KREQA-KZERO])
      NUTRIE_2 = NZERO+YPRATA*(PBAS-PZERO)/YNRATD
      NUTRIE_3 = NZERO+(PBAS-PZERO)*(2.*YPRATD/YNRATA-YPRATA/YNRATD)
      IF NBAS LE NUTRIE_2 THEN BEGIN
        NPUPT = NBAS
      ENDIF ELSE BEGIN
        IF NBAS LT NUTRIE_3 THEN BEGIN
          NPUPT = NBAS-0.25*(NBAS-NZERO-YPRATA*(PBAS-PZERO)/YNRATD)^2.0 /((YPRATD/YNRATA-YPRATA/YNRATD)*(PBAS-KZERO))
        ENDIF ELSE BEGIN
          NPUPT = NZERO+YPRATD*(PBAS-PZERO)/YNRATA
        ENDELSE
      ENDELSE
      NUTRIE_8 = NZERO+YKRATA*(KBAS-KZERO)/YNRATD
      NUTRIE_9 = NZERO+(KBAS-KZERO)*(2.*YKRATD/YNRATA-YKRATA/YNRATD)
      IF NBAS LE NUTRIE_8 THEN BEGIN
        NKUPT = NBAS
      ENDIF ELSE BEGIN
        IF NBAS LT NUTRIE_9 THEN BEGIN
          NKUPT = NBAS-0.25*(NBAS-NZERO-YKRATA*(KBAS-KZERO)/YNRATD)^2.0 /((YKRATD/YNRATA-YKRATA/YNRATD)*(KBAS-KZERO))
        ENDIF ELSE BEGIN
          NKUPT = NZERO+YKRATD*(KBAS-KZERO)/YNRATA
        ENDELSE
      ENDELSE
      NPUPT = MAX([0.,NPUPT])
      NKUPT = MAX([0.,NKUPT])
      NUPT  = MIN([NPUPT,NKUPT])
      NUTRIE_4 = PZERO+YNRATA*(NBAS-NZERO)/YPRATD
      NUTRIE_5 = PZERO+(NBAS-NZERO)*(2.*YNRATD/YPRATA-YNRATA/YPRATD)
      IF PBAS LE NUTRIE_4 THEN BEGIN
        PNUPT = PBAS
      ENDIF ELSE BEGIN
        IF PBAS LT NUTRIE_5 THEN BEGIN
          PNUPT = PBAS-0.25*(PBAS-PZERO-YNRATA*(NBAS-NZERO)/YPRATD)^2.0 /((YNRATD/YPRATA-YNRATA/YPRATD)*(NBAS-NZERO))
        ENDIF ELSE BEGIN
          PNUPT = PZERO+YNRATD*(NBAS-NZERO)/YPRATA
        ENDELSE
      ENDELSE
      NUTRIE_6 = PZERO+YKRATA*(KBAS-KZERO)/YPRATD
      NUTRIE_7 = PZERO+(KBAS-KZERO)*(2.*YKRATD/YPRATA-YKRATA/YPRATD)
      IF PBAS LE NUTRIE_6 THEN BEGIN
        PKUPT = PBAS
      ENDIF ELSE BEGIN
        IF PBAS LT NUTRIE_7 THEN BEGIN
          PKUPT = PBAS-0.25*(PBAS-PZERO-YKRATA*(KBAS-KZERO)/YPRATD)^2.0 / ((YKRATD/YPRATA-YKRATA/YPRATD)*(KBAS-KZERO))
        ENDIF ELSE BEGIN
          PKUPT = PZERO+YKRATD*(KBAS-KZERO)/YPRATA
        ENDELSE
      ENDELSE
      PNUPT = MAX([0.,PNUPT])
      PKUPT = MAX([0.,PKUPT])
      PUPT  = MIN([PNUPT,PKUPT])
      NUTRIE_10 = KZERO+YNRATA*(NBAS-NZERO)/YKRATD
      NUTRIE_11 = KZERO+(NBAS-NZERO)*(2.*YNRATD/YKRATA-YNRATA/YKRATD)
      IF KBAS LE NUTRIE_10 THEN BEGIN
        KNUPT = KBAS
      ENDIF ELSE BEGIN
        IF NUTRIE_11 LT NUTRIE_11 THEN BEGIN
          KNUPT = KBAS-0.25*(KBAS-KZERO-YNRATA*(NBAS-NZERO)/YKRATD)^2.0 /((YNRATD/YKRATA-YNRATA/YKRATD)*(NBAS-NZERO))
        ENDIF ELSE BEGIN
          KNUPT = KZERO+YNRATD*(NBAS-NZERO)/YKRATA
        ENDELSE
      ENDELSE
      NUTRIE_12 = KZERO+YPRATA*(PBAS-PZERO)/YKRATD
      NUTRIE_13 = KZERO+(PBAS-PZERO)*(2.*YPRATD/YKRATA-YPRATA/YKRATD)
      IF KBAS LE NUTRIE_12 THEN BEGIN
        KPUPT = KBAS
      ENDIF ELSE BEGIN
        IF KBAS LT NUTRIE_13 THEN BEGIN
          KPUPT = KBAS-0.25*(KBAS-KZERO-YPRATA*(PBAS-PZERO)/YKRATD)^2.0 /((YPRATD/YKRATA-YPRATA/YKRATD)*(PBAS-PZERO))
        ENDIF ELSE BEGIN
          KPUPT = KZERO+YPRATD*(PBAS-PZERO)/YKRATA
        ENDELSE
      ENDELSE
      KNUPT = MAX([0.,KNUPT])
      KPUPT = MAX([0.,KPUPT])
      KUPT  = MIN([KNUPT,KPUPT])
      YNA = YNRATA*MAX([0.,NUPT-NZERO])
      YND = YNRATD*MAX([0.,NUPT-NZERO])
      YPA = YPRATA*MAX([0.,PUPT-PZERO])
      YPD = YPRATD*MAX([0.,PUPT-PZERO])
      YKA = YKRATA*MAX([0.,KUPT-KZERO])
      YKD = YKRATD*MAX([0.,KUPT-KZERO])
      LYD = MIN([YND,YPD,YKD,YLDO])
      IF YND LE YPA THEN BEGIN
        EYNP = YND
        IF YND GT LYD THEN EYNP = LYD
        GOTO,JUMP6
      ENDIF ELSE BEGIN
        IF YNA GE LYD OR YND LE LYD THEN BEGIN
          IF YNA LT LYD AND YND EQ LYD THEN BEGIN
            LYD = MIN([YPD,YKD,YLDO])
          ENDIF ELSE BEGIN
            EYNP = LYD
            GOTO,JUMP6
          ENDELSE
        ENDIF
      ENDELSE
      EYNP = YPA+2.*(LYD-YPA)*(NUPT-NZERO-YPA/YNRATD)/(LYD/YNRATA-YPA/YNRATD)-(LYD-YPA)*(NUPT-NZERO-YPA/YNRATD)^2.0/(LYD/YNRATA-YPA/YNRATD)^2.0
      IF YPA GT LYD THEN EYNP = LYD
      JUMP6:LYD = MIN([YND,YPD,YKD,YLDO])                            
      IF YPD LE YNA THEN BEGIN
        EYPN = YPD
        IF YPD GT LYD THEN EYPN = LYD
        GOTO,JUMP7
      ENDIF ELSE BEGIN
        IF YPA GE LYD OR YPD LE LYD THEN BEGIN
          IF YPA LT LYD AND YPD EQ LYD THEN BEGIN
            LYD = MIN([YND,YKD,YLDO])
          ENDIF ELSE BEGIN
            EYPN = LYD
            GOTO,JUMP7
          ENDELSE
        ENDIF
      ENDELSE
      EYPN = YNA+2.*(LYD-YNA)*(PUPT-PZERO-YNA/YPRATD)/(LYD/YPRATA-YNA/YPRATD)-(LYD-YNA)*(PUPT-PZERO-YNA/YPRATD)^2.0/(LYD/YPRATA-YNA/YPRATD)^2.0
      IF YNA GT LYD THEN EYPN = LYD
      JUMP7:LYD = MIN([YND,YPD,YKD,YLDO])                              
      IF YNA LE YKA THEN BEGIN
        EYNK = YND
        IF YND GT LYD THEN EYNK = LYD
        GOTO,JUMP8
      ENDIF ELSE BEGIN
        IF YNA GE LYD OR YND LE LYD THEN BEGIN
          IF YNA LT LYD AND YND EQ LYD THEN BEGIN
            LYD = MIN([YPD,YKD,YLDO])
          ENDIF ELSE BEGIN
            EYNK = LYD
            GOTO,JUMP8
          ENDELSE
        ENDIF
      ENDELSE
      EYNK = YKA+2.*(LYD-YKA)*(NUPT-NZERO-YKA/YNRATD)/(LYD/YNRATA-YKA/YNRATD)-(LYD-YKA)*(NUPT-NZERO-YKA/YNRATD)^2.0/(LYD/YNRATA-YKA/YNRATD)^2.0
      IF YNA GT LYD THEN EYNK = LYD
      JUMP8:LYD = MIN([YND,YPD,YKD,YLDO])
      IF YKD LE YNA THEN BEGIN
        EYKN = YKD
        IF YKD GT LYD THEN EYKN = LYD
        GOTO,JUMP9
      ENDIF ELSE BEGIN
        IF YKA GE LYD OR YKD LE LYD THEN BEGIN
          IF YKA LT LYD AND YKD EQ LYD THEN BEGIN
            LYD = MIN([YND,YPD,YLDO])
          ENDIF ELSE BEGIN
            EYKN = LYD
            GOTO,JUMP9
          ENDELSE
        ENDIF
      ENDELSE
      EYKN = YNA+2.*(LYD-YNA)*(KUPT-KZERO-YNA/YKRATD)/(LYD/YKRATA-YNA/YKRATD)-(LYD-YNA)*(KUPT-KZERO-YNA/YKRATD)^2.0/(LYD/YKRATA-YNA/YKRATD)^2.0
      IF YNA GT LYD THEN EYKN = LYD
      JUMP9:LYD = MIN([YND,YPD,YKD,YLDO])
      IF YPD LE YKA THEN BEGIN
        EYPK = YPD
        IF YPD GT LYD THEN EYPK = LYD
        GOTO,JUMP10
      ENDIF ELSE BEGIN
        IF YPA GE LYD OR YPD LE LYD THEN BEGIN
          IF YPA LT LYD AND YPD EQ LYD THEN BEGIN
            LYD = MIN([YND,YKD,YLDO])
          ENDIF ELSE BEGIN
            EYPK = LYD
            GOTO,JUMP10
          ENDELSE
        ENDIF
      ENDELSE
      EYPK = YKA+2.*(LYD-YKA)*(PUPT-PZERO-YKA/YPRATD)/(LYD/YPRATA-YKA/YPRATD)-(LYD-YKA)*(PUPT-PZERO-YKA/YPRATD)^2.0/(LYD/YPRATA-YKA/YPRATD)^2.0
      IF YKA GT LYD THEN EYPK = LYD
      JUMP10:LYD = MIN([YND,YPD,YKD,YLDO])
      IF YKD LE YPA THEN BEGIN
        IF YKD GT LYD THEN EYKP = LYD
        GOTO,JUMP11
      ENDIF ELSE BEGIN
        IF YKA GE LYD OR YKD LE LYD THEN BEGIN
          IF YKA LT LYD AND YKD EQ LYD THEN BEGIN
            LYD = MIN([YND,YPD,YLDO])
          ENDIF ELSE BEGIN
            EYKP = LYD
            GOTO, JUMP11
          ENDELSE
        ENDIF
      ENDELSE
      EYKP = YPA+2.*(LYD-YPA)*(KUPT-KZERO-YPA/YKRATD) /(LYD/YKRATA-YPA/YKRATD)-(LYD-YPA)*(KUPT-KZERO-YPA/YKRATD)^2.0/(LYD/YKRATA-YPA/YKRATD)^2.0
      IF YPA GT LYD THEN EYKP = LYD
      JUMP11:LYD = MIN([YND,YPD,YKD,YLDO])              
      YE = MIN([LYD,(EYNP+EYPN+EYNK+EYKN+EYPK+EYKP)/6.0])
      IF IORGAN EQ 3 THEN BEGIN
        SOBAS1 = YE
        LVBAS1 = (LVOPT/(LVOPT+STOPT)*((STOPT+LVOPT-YZERO)*YE/SOOPT+YZERO))
        IF YE LE 0.0 THEN BEGIN
          LVBAS1 = LVOPT/(LVOPT+STOPT)*MIN([YZERO,NUPT/(0.333*(NMAXVE+2.*NMINVE)*(1.-NFIX)),PUPT/(0.333*(PMAXVE+2.*PMINVE)),KUPT/(0.333*(KMAXVE+2.*KMINVE))])
        ENDIF
        STBAS1 = (STOPT/LVOPT)*LVBAS1
      ENDIF ELSE BEGIN
        IF IORGAN NE 2 THEN BEGIN
          LVBAS1 = YE
          STBAS1 = (STOPT/LVOPT)*LVBAS1
          SOBAS1 = (SOOPT/LVOPT)*LVBAS1
        ENDIF ELSE BEGIN
          STBAS1 = YE
          LVBAS1 = (LVOPT/STOPT)*STBAS1
          SOBAS1 = (SOOPT/STOPT)*STBAS1
        ENDELSE
      ENDELSE
      RATBAS = SOBAS1/MAX([1.,LVBAS1+STBAS1])
      HIBAS  = SOBAS1/MAX([1.,LVBAS1+STBAS1+SOBAS1])
      NUTRIE_data[i_ns,i_nl] = YE
    ENDFOR
  ENDFOR
  NUTRIE_data1 = NUTRIE_data
  RETURN,NUTRIE_data1
END
PRO yield_NUTRIE,yield_nu,DUROPT,WSO_DATA,WST_DATA,WLV_data,ns,nl,crop_type,TWST_DATA1,TWLV_DATA1,TWSO_DATA1,TWRT_DATA1,RD,parameter_txt_path
  COMPILE_OPT idl2
  yield_nu = CAL_NUTRIE(DUROPT,parameter_txt_path,WSO_DATA,WST_DATA,WLV_data,ns,nl,crop_type,TWST_DATA1,TWLV_DATA1,TWSO_DATA1,TWRT_DATA1,RD)
END
