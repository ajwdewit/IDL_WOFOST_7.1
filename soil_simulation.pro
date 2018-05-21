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
;The module can simulate the process of soil such as root growth,soil water balance,and evapotranspire
;corresponds to the CROPNO.FOR,PENMAN.FOR,RNDIS.FOR,RNGEN.FOR,RNREAL.FOR,ROOTD.FOR,SUBSOL.FOR,WATFD.FOR,WATGW.FOR,WATPP.FOR,WOFSIM.FOR modules of orginal WOFOST in fortran )
FUNCTION CAL_PENMAN,Ju,latitude,AVRAD,ELEV,ANGSTA, ANGSTB,TMIN,TMAX,TEMP,VAPOUR,WIND
  COMPILE_OPT idl2
  VAP = VAPOUR
  PSYCON=0.67
  REFCFW=0.05
  REFCFS=0.15
  REFCFC=0.25
  LHVAP=2.45*1000000
  STBC=4.9*0.001
  TMPA  = (TMIN+TMAX)/2.
  TDIF  = TMAX - TMIN
  lim_min = 0.0
  lim_max = 1.0
  lim_x = (TDIF-12.)/4
  BU1 = CAL_LIMIT(lim_min,lim_max,lim_x)
  BU = 0.54 + 0.35 * BU1
  PBAR  = 1013.*EXP (-0.034*ELEV/(TMPA+273.))
  GAMMA = PSYCON*PBAR/1013.
  SVAP  = 6.10588 * EXP(17.32491*TMPA/(TMPA+238.102))
  DELTA = 238.102*17.32491*SVAP/(TMPA+238.102)^2
  VAP   = VAP < SVAP
  ;    VAP   = MIN([VAP,SVAP])
  ATMTR_PEMAN = CAL_ASTRO(Ju,latitude,AVRAD)
  ;  ATMTR = ATMTR_PEMAN.ASTRO_data1
  ATMTR =  ATMTR_PEMAN.ASTRO_data1
  ANGSTA = 0.25
  ANGSTB = 0.5
  lim_min = 0.0
  lim_max = 1.0
  lim_x = (ATMTR-ABS(ANGSTA))/ABS(ANGSTB)
  RELSSD = CAL_LIMIT(lim_min,lim_max,lim_x)
  RB  = STBC*(TMPA+273.0)^4*(0.56-0.079*SQRT(VAP))*(0.1+0.9*RELSSD)
  RNW = (AVRAD*(1.-REFCFW)-RB)/LHVAP
  RNS = (AVRAD*(1.-REFCFS)-RB)/LHVAP
  RNC = (AVRAD*(1.-REFCFC)-RB)/LHVAP
  max_data1 = 0.>(SVAP-VAP)
  EA  = 0.26 * max_data1 * (0.5+BU*WIND)
  EAC = 0.26 * max_data1 * (1.0+BU*WIND)
  ;  EA  = 0.26 * MAX([0.,(SVAP-VAP)]) * (0.5+BU*WIND)
  ;  EAC = 0.26 * MAX([0.,(SVAP-VAP)]) * (1.0+BU*WIND)
  E0  = (DELTA*RNW+GAMMA*EA)/(DELTA+GAMMA)/10
  ES0 = (DELTA*RNS+GAMMA*EA)/(DELTA+GAMMA)/10
  ET0 = (DELTA*RNC+GAMMA*EAC)/(DELTA+GAMMA)/10
  ET = [E0,ES0,ET0]
  ET = {ET1:E0,ET2:ES0,ET3:ET0}
  RETURN,ET
END

FUNCTION CAL_ROOTD,RDI,RRI,RDMCR,IWB,RDMSOL,FR,DELT,IAIRDU
  ZTI = 999
  RDMO = RDI
  RD = RDI
  IF IWB EQ 0 THEN BEGIN
    RDM = RDI > RDMCR
    ;    RDM = MAX([RDI,RDMCR])
    RDMO = 0.0
  ENDIF ELSE BEGIN
    ROOTD_1 = RDMSOL < RDMO < RDMCR
    ;        ROOTD_1 = MIN([RDMSOL,RDMO,RDMCR])
    MIN_data = ROOTD_1 < RDMCR
    RDM = RDI > MIN_data
  ;       RDM = MAX([RDI,MIN([ROOTD_1,RDMCR])])
  ENDELSE
  ZT = 999
  ROOTD_1 = RDM-RD
  ROOTD_2 = RRI*DELT
  RR = ROOTD_1 < ROOTD_2
  ;RR = MIN([ROOTD_1,ROOTD_2])
  IF FR LE 0 THEN BEGIN
    RR = 0.0
  ENDIF
  ROOTD_4 = ZT-RD
  IF IAIRDU EQ 0.0 AND ROOTD_4 LT 10.0 THEN BEGIN
    RR = 0.0
  ENDIF
  RD = RD +RR
  IF IWB EQ 0 THEN BEGIN
    RDMO = RD
  ENDIF
  RETURN,RD
END
FUNCTION CAL_SUBSOL,PF,ZTMRD,CONTAB1,CONTAB2
  COMPILE_OPT idl2
  ELOG10 = 2.302585
  D = ZTMRD
  PGAU = [0.1127016654,0.5,8872983346]
  WGAU = [0.2777778, 0.4444444, 0.2777778]
  START = [0.0,45.0,170.0,330.0]
  LOGST4 = 2.518514
  PFSTAN = [0.705143,1.352183,1.601282,1.771497,2.031409,2.192880,2.274233,2.397940,2.494110]
  DEL = FLTARR(4)
  PFGAU = FLTARR(12)
  HULP = FLTARR(12)
  CONDUC = FLTARR(12)
  START = FLTARR(4)
  PFSTAN = FLTARR(9)
  PGAU= FLTARR(3)
  PF1 = PF
  IINT = 0
  D1 =ZTMRD
  MH = EXP(ELOG10*PF1)
  IF PF1 LE 0 THEN BEGIN
    AF3 = -1
    AF1 = SMTAB1
    AF2 = SMTAB2
    GOTO,JUMP1
  ENDIF
  FOR i = 1,4 DO BEGIN
    IF i LE 3 THEN BEGIN
      SUBSOL_1 = START[i]
      min_data1 =SUBSOL_1 < MH
      DEL[i-1] = min_data1 - START[i-1]
    ;            DEL[i-1] = MIN([SUBSOL_1,MH]) - START[i-1]
    ENDIF
    IF i GE 4 THEN BEGIN
      DEL[i-1] = PF1 - LOGST4
    ENDIF
    IF DEL[i-1] LE 0 THEN BEGIN
      GOTO,JUMP2
      IINT = IINT +1
    ENDIF
  ENDFOR
  JUMP2: FOR i  = 1,IINT DO BEGIN
    FOR i2 = 1,3 DO BEGIN
      i3 = 3*(i-1) +i2
      IF i EQ IINT THEN BEGIN
        GOTO,JUMP3
      ENDIF
      PFGAU[I3-1] = PFSTAN[I3-1]
      GOTO,JUMP4
      JUMP3:SUBSOL_j =1
      IF IINT LE 3 THEN PFGAU[i3-1] = ALOG10(START[IINT-1]+PGAU[i2-1]*DEL[IINT-1])
      IF IINT EQ 4 THEN PFGAU[i3-1] = LOGST4 + PGAU[i2-1]*DEL[IINT-1]
      AF3 = PFGAU[i3-1]
      AF1 = CONTAB1
      AF2 = CONTAB2
      SUBSOL_2 = CAL_AFGEN(AF1,AF2,AF3)
      CONDUC[i3-1] = EXP(ELOG10*SUBSOL_2)
      HULP[i3-1] = DEL[i-1]*WGAU[i2-1]*CONDUC[i3-1]
      IF i3 GT 9 THEN HULP[i3-1] = HULP[i3-1]*ELOG10*EXP(ELOG10*PFGAU[i3-1])
    ENDFOR
  ENDFOR
  FU = 1.27
  AF3 = PF1
  AF1 = CONTAB1
  AF2 = CONTAB2
  SUBSOL_3 = CAL_AFGEN(AF1,AF2,AF3)
  FL = -1*EXP(ELOG10*SUBSOL_3)
  IF MH LE D1 THEN FU = 0.0
  IF MH GE D1 THEN FL = 0.0
  IF MH EQ D1 THEN GOTO, JUMP4
  IMAX = 3 *IINT
  FOR i = 1,15 DO BEGIN
    FLW = (FU + FL)/2
    DF = (FU - FL)/2
    SUBSOL_4 = DF / ABS(FLW)
    IF DF LT 0.01 AND SUBSOL_4 LT 0.1 THEN GOTO,JUMP4
    z = 0.0
    FOR i2 = 1,IMAX DO BEGIN
      z = z +HULP[i2-1]/(CONDUC[i2-1] + FLW)
    ENDFOR
    IF z GE D1 THEN FL = FLW
    IF z LE D1 THEN FU = FLW
  ENDFOR
  JUMP4:FLOW = (FU+FL)/2
  JUMP1: K0 = EXP (ELOG10 * CAL_AFGEN(AF1,AF2,AF3))
  FLOW = K0*(MH/D-1.)
  SUBSOL_5 = [K0,FLOW]
  RETURN, SUBSOL_5
END

FUNCTION CAL_WATGW,DELT,IDEM,RAIN,IWB,IOX,IAIRDU,KDIF,DEPNR,E0,ES0,ET0,LAI,CRAIRC,SMW,SM_t,RDI,RD,TRA,EVWMX,EVSMX,RAINT,SMFCF,SM0,SSI,XDEF,IDRAIN,DD,SMTAB1,SMTAB2,CONTAB1,CONTAB2,RRI,RDMCR,RDMSOL,SOPE,KSUB,ZTI,CFET
  COMPILE_OPT idl2
  ;计算土壤水平衡
  ;土壤参数输入区
  ;-------------------------------------------------------------------------------------------
;  SMFCF = 0.460                                                                                ;田间持水量
;  SMW = 0.200                                                                                  ;凋萎点田间含水量
;  SM0 = 0.570                                                                                  ;饱和田间含水量
;  SSI = 0.0                                                                                    ;土壤表面积水状况
;  XDEF = 16000                                                                                 ;土壤含水层最大深度
;  ZTI = 999                                                                                    ;初始地下水层的深度
;  IDRAIN = 0.0                                                                                 ;是否有地下排水设施
;  DD = 0.0                                                                                     ;地下排水设施深度
;  SMTAB1 =[-1.000,1.000,1.300,1.491,2.000,2.400,2.700,3.400,4.204,6.000]
;  SMTAB2 =[0.570,0.533,0.524,0.515,0.486,0.451,0.420,0.350,0.300,0.270]                        ;土壤水分平衡
;  CONTAB1 = [0.000,1.000,1.300,1.491,1.700,2.000,2.400,2.700,3.000,3.400,3.700,4.000,4.204]    ;土壤导水率
;  CONTAB2 = [1.033,-0.824,-1.155,-1.398,-1.523,-1.959,-2.495,-2.886,-3.276,-3.770,-4.131,-4.481,-4.745]
;  RRI  = 2.2                                                                                   ;最大根系生长速度
;  RDMCR = 100                                                                                  ;根系最大深度
;  RDMSOL = 200                                                                                 ;土壤允许的最大根系深度
; SMFCF,SM0,SSI,XDEF,IDRAIN,DD,SMTAB1,SMTAB2,CONTAB1,CONTAB2,RRI,RDMCR,RDMSOL,SOPE,KSUB,ZTI,CFET
;  SOPE = 0.55                                                                                  ;最大根系透水率
;  KSUB = 0.37                                                                                  ;最大底层透水率
  CFET = 1.0                                                                                   ;农田蒸散校正系数
  SSMAX = 0.0                                                                                  ;最大地表存水量
  ;  SMTAB = FLTARR(30)
  NOTINF = 0.0
  PFTAB = FLTARR(30)
  CONTAB = FLTARR(30)
  SDEFTB = FLTARR(30)
  DEFDTB=FLTARR(30)
  PGAU = FLTARR(3)
  NINFTB = FLTARR(20)
  IAIRDU = 0
  
  IFUNRN = 0.0
  TRAT   = 0.
  EVST   = 0.
  EVWT   = 0.
  TSR    = 0.
  CRT    = 0.
  PERCT  = 0.
  WDRT   = 0.
  TOTINF = 0.
  TOTIRR = 0.
  SUMSM  = 0.
  DRAINT = 0.
  EVS  = 0.
  EVW  = 0.
  RIN  = 0.
  RIRR = 0.
  DW   = 0.
  PERC = 0.
  CR   = 0.
  DMAX = 0.
  DZ   = 0.
  NINFTB = FLTARR(20)
  NINFTB1 = [0.0,0.5,1.5,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
  NINFTB2 = [0.0,0.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
  
  
  ;----------------------------------------------------------------------------------------------------------------------------
  SM = SM_t
  PGAU = [0.1127016654,0.5,0.8872983346]
  WGAU = [0.2777778, 0.4444444, 0.2777778]
  WATGW_1 = ALOG10(200)
  AF3 = WATGW_1
  AF1 = SMTAB1
  AF2 = SMTAB2
  SMFCF = CAL_AFGEN(AF1,AF2,AF3)
  WATGW_2 = ALOG10(16000)
  AF3 = WATGW_1
  SMW1 = CAL_AFGEN(AF1,AF2,AF3)
  AF3 = -1
  SM01 = CAL_AFGEN(AF1,AF2,AF3)
  AF1 = CONTAB1
  AF2 = CONTAB2
  WATGW_1 = CAL_AFGEN(AF1,AF2,AF3)
  K0 = 10.0^WATGW_1
  ILSM = N_ELEMENTS(SMTAB1)
  PFTAB1 = FLTARR(15)
  PFTAB2 = FLTARR(15)
  SDEFTB1 = FLTARR(15)
  SDEFTB2 = FLTARR(15)
  DEFDTB1 = FLTARR(15)
  DEFDTB2 = FLTARR(15)
  ;  WATGW_evtra = CAL_EVTRA(IWB,IOX,IAIRDU,KDIF,CFET,DEPNR,E0,ES0,ET0,LAI,CRAIRC,SM,SM0,SMFCF,SMW)
  ;  TRA = WATGW_evtra[0]
  FOR I_WATGW = 0,ILSM-1 DO BEGIN
    PFTAB1[ILSM+1-I_WATGW] = SMTAB1[I_WATGW]
    PFTAB2[ILSM+1-I_WATGW] = SMTAB2[I_WATGW]
  ENDFOR
  RTDF = 0.0
  RDOLD = RD
  MH0 = 0
  MH1 = 2
  SDEFTB1[0] = 0.0
  SDEFTB2[0] = 0.0
  DEFDTB1[0] = 0.0
  DEFDTB2[0] = 0.0
  FOR i = 1,14 DO BEGIN
    SDEFTB1[i]= MH1
    SDEFTB2[i] = SDEFTB2[i-1]
    FOR i3 = 0,2 DO BEGIN
      AF1 = CONTAB1
      AF2 = CONTAB2
      AF3 = MH0+(MH1-MH0)*PGAU[I3]
      WATGW_2 = CAL_AFGEN(AF1,AF2,AF3)
      SDEFTB2[i] = SDEFTB2[i]+WGAU[i3]*(MH1-MH0)*(SM01-WATGW_2)
    ENDFOR
    DEFDTB1[i] = SDEFTB2[i]
    DEFDTB2[i] = SDEFTB1[i]
    MH0 = MH1
    MH1 = 2*MH1
  ENDFOR
  SS = SSI
  lim_min = 0.1
  lim_max = XDEF
  lim_x = ZTI
  ZT = CAL_LIMIT(lim_min,lim_max,lim_x)
  IF IDRAIN EQ 1 THEN ZT = ZT > DD
  ;  IF IDRAIN EQ 1 THEN ZT = MAX([ZT,DD])
  AF1 =  SDEFTB1
  AF2 =  SDEFTB2
  AF3 =  ZT-RD
  SUBAIR = CAL_AFGEN(AF1,AF2,AF3)
  WZ  = (XDEF-RD)*SM01 - SUBAIR
  WZI = WZ
  AF1 = SDEFTB1
  AF2 = SDEFTB2
  AF3 = ZT
  WATGW_3 = CAL_AFGEN(AF1,AF2,AF3)
  WE = SM01*RD +SUBAIR - WATGW_3
  AF3 = DD
  WATGW_4 = CAL_AFGEN(AF1,AF2,AF3)
  WEDTOT = SM01 *DD - WATGW_4
  WATGW_5 = RD +100
  IF ZT LT WATGW_5 THEN BEGIN
    W = WE
  ENDIF ELSE BEGIN
    W = SMFCF *RD
  ENDELSE
  SM = W/RD
  WI = W
  DSLR = 1.
  AF3 = 3.0
  AF1 = SMTAB1
  AF2 = SMTAB2
  WATGW_6 = CAL_AFGEN(AF1,AF2,AF3)
  IF SM LE WATGW_6 THEN BEGIN
    DSLR = 5.0
  ENDIF
  
  
  ;  WATGW_7 = CAL_EVTRA(IWB,IOX,IAIRDU,KDIF,CFET,DEPNR,E0,ES0,ET0,LAI,CRAIRC,SM,SM0,SMFCF,SMW)
  ;  EVWMX = WATGW_7[2]
  ;  EVSMX = WATGW_7[3]
  IF SS GT 1 THEN BEGIN
    EVW = EVWMX
  ENDIF ELSE BEGIN
    IF RIN GE 1 THEN BEGIN
      EVS = EVWMX
      DSLR = 1.0
    ENDIF ELSE BEGIN
      DSLR = DSLR +1
      EVSMXT = EVSMX*(SQRT(DSLR)-SQRT(DSLR-1.))
      WATGW_8 = EVSMXT+RIN
      EVS = EVSMX < (EVSMXT+RIN)
    ;            EVS = MIN([EVSMX,EVSMXT+RIN])
    ENDELSE
  ENDELSE
  IF SS GT 0.1 THEN BEGIN
    AVAIL = SS +(RAIN * (1.-NOTINF)+RIRR-EVW) * DELT
    WATGW_9 = SOPE*DELT
    min_data1 = WATGW_9 < AVAIL
    RINPRE = min_data1/DELT
  ;        RINPRE = MIN([WATGW_9,AVAIL])/DELT
  ENDIF ELSE BEGIN
    IF IFUNRN EQ 0.0 THEN RINPRE =(1.-NOTINF)*RAIN+RIRR+SS/DELT
    AF1 =  NINFTB1
    AF2 =  NINFTB2
    AF3 = RAIN
    RINPRE_data = CAL_AFGEN(AF1,AF2,AF3)
    IF IFUNRN EQ 1.0 THEN RINPRE  = (1.-NOTINF*RINPRE_data )*RAIN+RIRR+SS/DELT
    
  ENDELSE
  ZTMRD = ZT-RD
  CR   = 0.
  PERC = 0.
  IF ZTMRD GT 0.0 THEN BEGIN
    AF1 = SDEFTB1
    AF2 = SDEFTB2
    AF3 = ZT
    WATGW_3 = CAL_AFGEN(AF1,AF2,AF3)
    WE = SM01*RD +SUBAIR - WATGW_3
    AF3 = SM
    AF1 = PFTAB1
    AF2 = PFTAB1
    PF = CAL_AFGEN(AF1,AF2,AF3)
    WATGW_subsol = CAL_SUBSOL(PF,ZTMRD,CONTAB1,CONTAB2)
    FLOW = WATGW_subsol[1]
    K0 = WATGW_subsol[0]
    WATGW_10 = we -w
    WATGW_11 = WATGW_10 > 0.0
    ;        WATGW_11 = MAX([WATGW_10,0.0])
    WATGW_12 = WATGW_11/DELT
    WATGW_13 = WATGW_11 < 0.0
    ;        WATGW_13 = MIN([WATGW_11,0.0])
    WATGW_14 = WATGW_13/DELT
    WATGW_15 = 0.05*K0
    IF FLOW GE 0 THEN CR = FLOW < WATGW_12
    max_data1 = FLOW > WATGW_14
    IF FLOW LE 0.0 THEN PERC = -1*max_data1
    min_data1 = PERC < WATGW_15
    IF IAIRDU EQ 1 THEN PERC = min_data1
  ;        IF FLOW GE 0 THEN CR = MIN([FLOW,WATGW_12])
  ;        IF FLOW LE 0.0 THEN PERC = -1*MAX([FLOW,WATGW_14])
  ;        IF IAIRDU EQ 1 THEN PERC = MIN([PERC,WATGW_15])
  ENDIF
  IF IDRAIN EQ 1 AND ZT LT DD THEN BEGIN
    DR1 = 0.2*K0
    IF ZTMRD LE 0 THEN BEGIN
      WATGW_16 = DD -RD
      max_data1 = 0.0 > WATGW_16
      WATGW_17 = (max_data1*SM01 +W -WEDTOT)/DELT
      
      max_data1 = 0.0 > WATGW_17
      DR2 =max_data1/DELT
      DMAX = DR1 < DR2
    ;            WATGW_17 = (MAX([0.,WATGW_16])*SM01 +W -WEDTOT)/DELT
    ;            DR2 = MAX([0.0,WATGW_17])/DELT
    ;            DMAX = MIN([DR1,DR2])
    ENDIF ELSE BEGIN
      AF3 = DD - RD
      AF1 = SDEFTB1
      AF2 = SDEFTB2
      WATGW_18 = CAL_AFGEN(AF1,AF2,AF3)
      DR2 = (WATGW_18 - SUBAIR)/DELT
      DMAX = DR1 < DR2
    ;      DMAX = MIN([DR1,DR2])
    ENDELSE
  ENDIF ELSE BEGIN
    DMAX = 0.0
  ENDELSE
  IF ZTMRD LE 0.0 THEN BEGIN
    IF  ZT GE 0.1 THEN AIRC = (RD *SM01-W)/ZT
    PERC = DMAX
    WATGW_19 = AIRC*ZT/DELT + TRA + EVS + PERC
    RIN = RINPRE < WATGW_19
    ;    RIN = MIN([RINPRE,WATGW_19])
    DZ = (TRA+EVS+PERC-RIN)/AIRC
    WATGW_20 = DZ*DELT
    WATGW_21 = RD-ZT
    IF WATGW_20 GT WATGW_21 THEN BEGIN
      CR = (DZ*DELT-(RD-ZT))*AIRC/DELT
      AF1 =DEFDTB1
      AF2 = DEFDTB2
      AF3 = CR*DELT
      WATGW_29 = CAL_AFGEN(AF1,AF2,AF3)
      DZ = (WATGW_29 + RD-ZT)/DELT
    ENDIF
  ENDIF ELSE BEGIN
    DEF1 = SUBAIR + (DMAX + CR - PERC)*DELT
    IF DEF1 LT 0.0 THEN PERC = PERC + DEF1/DELT
    AF1 =DEFDTB1
    AF2 = DEFDTB2
    AF3 = DEF1
    WATGW_30 = CAL_AFGEN(AF1,AF2,AF3)
    DZ = (WATGW_30 + RD - ZT) /DELT
    WATGW_22 = (SM01-SM-0.0004)*RD/DELT+TRA+EVS+PERC-CR
    RIN = RINPRE < WATGW_22
  ;    RIN = MIN([RINPRE,WATGW_22])
  ENDELSE
  DW = - TRA -EVS - PERC + CR + RIN
  RAINT1 = RAINT
  EVW1   = EVW
  EVS1   = EVS
  SM1    = SM
  SS1    = SS
  ZT1    = ZT
  ;------------------------------------------------
  ;系统集成，求算生育期内的各个参数的累计量的和
  TRAT = TRAT + TRA*DELT
  EVWT = EVWT + EVW*DELT
  EVST = EVST + EVS*DELT
  RAINT  = RAINT + RAIN*DELT
  TOTINF = TOTINF + RIN*DELT
  TOTIRR = TOTIRR + RIRR*DELT
  SSPRE = SS + (RAIN+RIRR-EVW-RIN)*DELT
  SS    = SSPRE < SSMAX
  ;  SS    = MIN([SSPRE,SSMAX])
  TSR   = TSR + (SSPRE-SS)
  W = W + DW*DELT
  CRT   = CRT + CR*DELT
  PERCT = PERCT + PERC*DELT
  DRAINT = DRAINT + DMAX * DELT
  ZT = ZT + DZ*DELT
  AF3 = ZT-RDOLD
  AF1 = SDEFTB1
  AF2 = SDEFTB2
  SUBAIR = CAL_AFGEN(AF1,AF2,AF3)
  WZ = (XDEF-RDOLD)*SM01 - SUBAIR
  ;---------------------------------------------------
  ;根系范围改变之后的土壤水分模拟
  WATGW_22 = RD-RDOLD
  IF WATGW_22 GT 0.001 THEN BEGIN
    SUBAI0 = SUBAIR
    AF3 = ZT-RD
    AF1 = SDEFTB1
    AF2 = SDEFTB2
    SUBAIR = CAL_AFGEN(AF1,AF2,AF3)
    WZ = (XDEF-RD)*SM01 - SUBAIR
    WDR = SM01*(RD-RDOLD) - (SUBAI0-SUBAIR)
    WDRT = WDRT + WDR
    W = W + WDR
  ENDIF
  SM = W/RD
  SUMSM = SUMSM + SM*DELT
  RDOLD = RD
  IF IAIRDU EQ 0.0 AND RTDF GE 10 THEN BEGIN
    TERMNL = 1.0
  ENDIF ELSE BEGIN
    IF ZT LT 10 THEN RTDF = RTDF +1
    IF ZT GE 10 THEN RTDF = 0.0
  ENDELSE
  WBALRT = TOTINF + CRT + WI - W + WDRT - EVST - TRAT - PERCT
  WBALTT = SSI + RAINT + TOTIRR + WI - W + WZI - WZ - TRAT- EVWT - EVST - TSR - DRAINT - SS
  RETURN, SM
END
FUNCTION CAL_WATFD ,SSI,IAIRDU,SMW,RD,RDMCR,RIN,RAIN,NOTINF,DELT,TRA,EVSMX,K0,SMFCF,SM0,XDEF,IDRAIN,DD,SMTAB1,SMTAB2,CONTAB1,CONTAB2,RRI,RDMSOL,SOPE,KSUB,ZTI,CFET
  COMPILE_OPT idl2
  
  RDM = RDMCR
  NINFTB = FLTARR(20)
  NINFTB1 = [0.0,0.5,1.5,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
  NINFTB2 = [0.0,0.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
  
;  SMFCF = 0.460                                      ;田间持水量
;  SMW = 0.200                                        ;凋萎点含水量
;  SM0 = 0.570                                        ;饱和含水量
;  SSI = 0.0
;  XDEF = 16000
;  ZTI = 999
;  IDRAIN = 0.0
;  DD = 0.0
;  SMTAB1 =[-1.000,1.000,1.300,1.491,2.000,2.400,2.700,3.400,4.204,6.000]                                         ;土壤含水量
;  SMTAB2 =[0.570,0.533,0.524,0.515,0.486,0.451,0.420,0.350,0.300,0.270]
;  CONTAB1 = [0.000,1.000,1.300,1.491,1.700,2.000,2.400,2.700,3.000,3.400,3.700,4.000,4.204]                      ;土壤水分渗透系数
;  CONTAB2 = [1.033,-0.824,-1.155,-1.398,-1.523,-1.959,-2.495,-2.886,-3.276,-3.770,-4.131,-4.481,-4.745]
;  RRI  = 2.2                                                                                                      ;每天最大的根系生长系数
;  RDMCR = 100
;  RDMSOL = 200
;  
;  SOPE = 0.55
;  KSUB = 0.37
;  CFET = 1.0
  
  NOTINF = 0.0
  ;  SMTAB = FLTARR(30)
  PFTAB = FLTARR(30)
  CONTAB = FLTARR(30)
  SDEFTB = FLTARR(30)
  DEFDTB=FLTARR(30)
  PGAU = FLTARR(3)
  NINFTB = FLTARR(20)
  PFTAB1 = FLTARR(15)
  PFTAB2 = FLTARR(15)
  IAIRDU = 0
  SSMAX = 0.0
  IFUNRN = 0.0
  SMLIM = 0.04
  WAV = 20.0
  RIN = 0.0
  RIRR = 0.0
  SOPE = 0.55
  TRAT   = 0.
  EVST   = 0.
  EVWT   = 0.
  TSR    = 0.
  RAINT  = 0.
  WDRT   = 0.
  TOTINF = 0.
  TOTIRR = 0.
  SUMSM  = 0.
  PERCT  = 0.
  LOSST  = 0.
  TRAJ  = 0.
  EVSJ  = 0.
  EVWJ  = 0.
  RAINJ = 0.
  RINJ  = 0.
  RIRRJ = 0.
  DWJ   = 0.
  PERCJ = 0.
  LOSSJ = 0.
  DWLOWJ = 0.
  WWLOWJ = 0.
  CRJ    = 0.
  DMAXJ  = 0.
  DZJ    = 0.
  EVS   = 0.
  EVW   = 0.
  RAIN  = 0.
  RIN   = 0.
  RIRR  = 0.
  DW    = 0.
  PERC  = 0.
  LOSS  = 0.
  DWLOW = 0.
  SS = SSI
  RDOLD = RD
  IF SMLIM LT SMW THEN SMLIM = SMW
  IF SMLIM LT SM0 THEN SMLIM = SM0
  IF IAIRDU EQ 1.0 THEN SMLIM = SM0
  lim_min = SMW
  lim_max = SMLIM
  lim_x = SMW+WAV/RD
  SM = CAL_LIMIT(lim_min,lim_max,lim_x)
  W = SM *RD
  WI = W
  DSLR = 1.0
  WATFD_1 = SMW+0.5*(SMFCF-SMW)
  IF SM LE WATFD_1 THEN DSLR = 5.0
  lim_min = 0.0
  lim_max = SM0*(RDM-RD)
  lim_x = WAV+RDM*SMW-W
  WLOW = CAL_LIMIT(lim_min,lim_max,lim_x)
  WLOWI = WLOW
  WWLOW = W + WLOW
  EVW = 0.0
  EVS = 0.0
  IF SS GT 1.0 THEN BEGIN
    EVW = EVWMX
  ENDIF ELSE BEGIN
    IF RIN GE 1.0 THEN BEGIN
      EVS  = EVSMX
      DSLR = 1.
    ENDIF ELSE BEGIN
      DSLR   = DSLR+1.
      EVSMXT = EVSMX*(SQRT(DSLR)-SQRT(DSLR-1.))
      EVS    = EVSMX < (EVSMXT+RIN)
    ;      EVS    = MIN([EVSMX, EVSMXT+RIN])
    ENDELSE
  ENDELSE
  IF SS LE 0.1 THEN BEGIN
    IF IFUNRN EQ 0.0 THEN RINPRE = (1.-NOTINF)*RAIN+RIRR+SS/DELT
    AF1 =  NINFTB1
    AF2 =  NINFTB2
    AF3 = RAIN
    WATFD_2 = CAL_AFGEN(AF1,AF2,AF3)
    IF IFUNRN EQ 1.0 THEN RINPRE = (1.-NOTINF*WATFD_2)*RAIN+RIRR+SS/DELT
  ENDIF ELSE BEGIN
    AVAIL = SS + (RAIN * (1.-NOTINF)+RIRR-EVW) * DELT
    min_data2 = (SOPE*DELT) <  AVAIL
    RINPRE = min_data2/DELT
  ;    RINPRE = MIN([SOPE*DELT, AVAIL])/DELT
  ENDELSE
  WE = SMFCF * RD
  lim_min = 0.0
  lim_max = SOPE
  lim_x = (W - WE)/DELT - TRA - EVS
  PERC1 = CAL_LIMIT(lim_min,lim_max,lim_x)
  WELOW = SMFCF * (RDM - RD)
  lim_min = 0.0
  lim_max = KSUB
  lim_x = (WLOW - WELOW)/DELT + PERC1
  LOSS = CAL_LIMIT(lim_min,lim_max,lim_x)
  ;  ILSM = n_elements(SMTAB1)
  ;  for I_WATGW = 0,ILSM-1 do begin
  ;    PFTAB1[ILSM+1-I_WATGW] = SMTAB1[I_WATGW]
  ;    PFTAB2[ILSM+1-I_WATGW] = SMTAB2[I_WATGW]
  ;  endfor
  ;  AF3 = SM
  ;  AF1 = PFTAB1
  ;  AF2 = PFTAB1
  ;  PF = CAL_AFGEN(AF1,AF2,AF3)
  ;  ZTMRD = ZT-RD
  ;  WATGW_subsol = CAL_SUBSOL(PF,ZTMRD,CONTAB1,CONTAB2)
  ;  FLOW = WATGW_subsol[1]
  ;  K0 = WATGW_subsol[0]
  IF IAIRDU EQ 1.0 THEN LOSS = LOSS < 0.05*K0
  ;  IF IAIRDU EQ 1.0 THEN LOSS = MIN([LOSS, 0.05*K0])
  PERC2 = ((RDM -RD) * SM0 - WLOW) / DELT + LOSS
  PERC  = PERC1 < PERC2
  ;  PERC  = MIN([PERC1,PERC2])
  WATFD_3 = (SM0-SM)*RD/DELT + TRA + EVS + PERC
  RIN = RINPRE < WATFD_3
  ;  RIN = MIN([RINPRE,WATFD_3])
  DW    = - TRA - EVS - PERC + RIN
  DWLOW = PERC - LOSS
  TRAT = TRAT + TRA*DELT
  EVWT = EVWT + EVW*DELT
  EVST = EVST + EVS*DELT
  RAINT  = RAINT + RAIN*DELT
  TOTINF = TOTINF + RIN*DELT
  TOTIRR = TOTIRR + RIRR*DELT
  SSPRE = SS + (RAIN+RIRR-EVW-RIN)*DELT
  SS    = SSPRE < SSMAX
  ;  SS    = MIN([SSPRE, SSMAX])
  TSR   = TSR + (SSPRE-SS)
  W_NEW = W + DW*DELT
  IF W_NEW LT 0.0 THEN BEGIN
    EVST = EVST + W_NEW
    W = 0.0
  ENDIF ELSE BEGIN
    E = W_NEW
  ENDELSE
  PERCT = PERCT + PERC*DELT
  LOSST = LOSST + LOSS*DELT
  WLOW = WLOW + DWLOW*DELT
  WWLOW = W + WLOW
  IF RD-RDOLD GT 0.001 THEN BEGIN
    WDR  = WLOW*(RD-RDOLD)/(RDM-RDOLD)
    WLOW = WLOW - WDR
    WDRT = WDRT + WDR
    W = W + WDR
  ENDIF
  SM = W/RD
  SUMSM = SUMSM + SM*DELT
  RDOLD = RD
  RETURN,SM
END
PRO soil_simulation,I_stess,ISTATE,IWB,IOX,IAIRDU,KDIF,CFET,DEPNR,E0,ES0,ET0,LAI,CRAIRC,SM,SM0,SMW,SSI,RD,RDMCR,RIN,RAIN,NOTINF,DELT,EVSMX,K0,Ju,latitude,AVRAD,ELEV,ANGSTA,ANGSTB,TMIN,TMAX,TEMP,VAPOUR,WIND,RRI,RDMSOL,FR,IDEM,SM_t,RDI,TRA,EVWMX,RAINT,SMFCF,XDEF,IDRAIN,DD,SMTAB1,SMTAB2,CONTAB1,CONTAB2,SOPE,KSUB,ZTI
  COMPILE_OPT idl2
  CFET_DATA = CAL_PENMAN(Ju,latitude,AVRAD,ELEV,ANGSTA, ANGSTB,TMIN,TMAX,TEMP,VAPOUR,WIND)                                       ;计算作物蒸散
  ET0 = CFET_DATA.ET3
  E0 = CFET_DATA.ET1
  ES0 = CFET_DATA.ET2
  IF ISTATE EQ 3.0 THEN BEGIN
    RD = CAL_ROOTD(RDI,RRI,RDMCR,IWB,RDMSOL,FR,DELT,IAIRDU)                                                                       ;计算根系生长
    TRA_data11 = CAl_EVTRA(IWB,IOX,IAIRDU,KDIF,CFET,DEPNR,E0,ES0,ET0,LAI,CRAIRC,SM,SM0,SMFCF,SMW)
    TRA = TRA_data11.TRA_data1
    EVWMX = TRA_data11.TRA_data3
    EVSMX = TRA_data11.TRA_data4
  ENDIF ELSE BEGIN
    RD = RDI
    EVWMX = E0
    EVSMX = ES0
    TRA = 0.0
  ENDELSE
  IF I_stess EQ 2.0 THEN BEGIN
    SM_data = CAL_WATGW(DELT,IDEM,RAIN,IWB,IOX,IAIRDU,KDIF,DEPNR,E0,ES0,ET0,LAI,CRAIRC,SMW,SM_t,RDI,RD,TRA,EVWMX,EVSMX,RAINT,SMFCF,SM0,SSI,XDEF,IDRAIN,DD,SMTAB1,SMTAB2,CONTAB1,CONTAB2,RRI,RDMCR,RDMSOL,SOPE,KSUB,ZTI,CFET)     
    SM = SM_data
  ENDIF
  IF I_stess EQ 0.0 THEN SM = SMFCF
  IF I_stess EQ 1.0 THEN BEGIN
    SM_data = CAL_WATFD(SSI,IAIRDU,SMW,RD,RDMCR,RIN,RAIN,NOTINF,DELT,TRA,EVSMX,K0,SMFCF,SM0,XDEF,IDRAIN,DD,SMTAB1,SMTAB2,CONTAB1,CONTAB2,RRI,RDMSOL,SOPE,KSUB,ZTI,CFET)                                               
    SM = SM_data
  ENDIF
  SM_t  = SM
  RDI = RD
  CFET = 1.0
  TRA_data = CAl_EVTRA(IWB,IOX,IAIRDU,KDIF,CFET,DEPNR,E0,ES0,ET0,LAI,CRAIRC,SM,SM0,SMFCF,SMW)                                     
  TRA = TRA_data.TRA_data1
  TRAMX = TRA_data.TRA_data2
  
  index_SIOL1=WHERE(TRA EQ TRAMX AND I_STESS GT 0.0 ,count_SIOL1)
  IF count_SIOL1 GT 0 THEN BEGIN
    TRA[index_SIOL1] = TRA[index_SIOL1] * 0.95
  ENDIF
;  IF TRA EQ TRAMX AND I_STESS GT 0.0 THEN TRA = TRA *0.95
END