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
;(The fuction of this part is to caculate assimilation rate and return the total daily assimilation amout to the main project
; corresponds to the ASSIM.FOR,ASTRO.FOR,TOTASS.FOR, modules of orginal WOFOST in fortran)
FUNCTION CAL_DAYL,Ju,latitude
  COMPILE_OPT idl2
  PI= 3.1415926
  RAD = 0.0174533
  DEC = -ASIN(SIN(23.45*RAD)*COS(2.*PI*(Ju+10.)/365.))
  SC  = 1370.*(1.+0.033*COS(2.*PI*Ju/365.))
  DECA = DEC
  ;SSSSSSS
  SCA  = SC
  SINLD = SIN(RAD*latitude)*SIN(DECA)
  COSLD = COS(RAD*latitude)*COS(DECA)
  AOB   = SINLD/COSLD
  DAYL_data1  = 12.0*(1.+2.*ASIN(AOB)/PI)
  ;    DAYL_data = [DAYL_data1,SC]
  DAYL_data = {SC_data:SCA,DAYL_data_S:DAYL_data1}
  RETURN, DAYL_data
END
FUNCTION CAL_ASTRO,Ju,latitude,AVRAD
  COMPILE_OPT idl2
  PI= 3.1415926
  RAD = 0.0174533
  ANGLE = -4
  DEC = -ASIN(SIN(23.45*RAD)*COS(2.*PI*(Ju+10.)/365.))
  SC  = 1370.*(1.+0.033*COS(2.*PI*Ju/365.))
  DECA = DEC
  SCA  = SC
  SINLD = SIN(RAD*latitude)*SIN(DECA)
  COSLD = COS(RAD*latitude)*COS(DECA)
  AOB   = SINLD/COSLD
  DAYLP = 12.0*(1.+2.*ASIN((-SIN(ANGLE*RAD)+SINLD)/COSLD)/PI)
  DAYL_AS = CAL_DAYL(Ju,latitude)
  DAYL = DAYL_AS.DAYL_data_S
  DSINB  = 3600.*(DAYL*SINLD+24.*COSLD*SQRT(1.-AOB^2)/PI)
  DSINBE = 3600.*(DAYL*(SINLD+0.4*(SINLD^2+COSLD^2*0.5))+12.*COSLD*(2.+3.*0.4*SINLD)*SQRT(1.-AOB^2)/PI)
  ANGOT  = SCA*DSINB
  ATMTR  = AVRAD/ANGOT
  ;    ASTRO_data =[ATMTR,SINLD,COSLD,DSINBE]
  ASTRO_data = {ASTRO_data1:ATMTR,ASTRO_data2:SINLD,ASTRO_data3:COSLD,ASTRO_data4:DSINBE}
  RETURN,ASTRO_data
END
FUNCTION CAL_DIFPP,Ju,latitude,AVRAD,SCA
  COMPILE_OPT idl2
  ATMTR_data = CAL_ASTRO(Ju,latitude,AVRAD)
  ATMTR = ATMTR_data.ASTRO_data1
  ;---------------------------------------------------------------------------------------
  ;数组模式
  
  FRDIF = ATMTR *0.0
  index1=WHERE(ATMTR LT 0.07,count1)
  IF count1 GT 0 THEN BEGIN
    FRDIF[index1] = 1.0
  ENDIF
  
  index2=WHERE(ATMTR GE 0.07 AND ATMTR LE 0.35 ,count2)
  IF count2 GT 0 THEN BEGIN
    FRDIF[index2] = 1.0-2.3*(ATMTR[index2]-0.07)^2
  ENDIF
  
  index3=WHERE(ATMTR GT 0.35 AND ATMTR LE 0.75,count3)
  IF count3 GT 0 THEN BEGIN
    FRDIF[index3] = 1.33-1.46*ATMTR[index3]
  ENDIF
  
  index4=WHERE(ATMTR GT 0.75,count4)
  IF count4 GT 0 THEN BEGIN
    FRDIF[index4] = 0.23
  ENDIF
  ;    IF ATMTR GT 0.75 THEN BEGIN
  ;        FRDIF = 0.23
  ;    ENDIF ELSE BEGIN
  ;        IF  ATMTR GT 0.35 THEN BEGIN
  ;            FRDIF = 1.33-1.46*ATMTR
  ;        ENDIF ELSE BEGIN
  ;            IF ATMTR GE 0.07 THEN BEGIN
  ;                FRDIF = 1.0-2.3*(ATMTR-0.07)^2
  ;            ENDIF ELSE BEGIN
  ;                FRDIF = 1.0
  ;            ENDELSE
  ;        ENDELSE
  ;    ENDELSE
  
  DIFPP_data = FRDIF*ATMTR*0.5*SCA
  RETURN,DIFPP_data
END
FUNCTION CAL_ASSIM,AMAX,EFF,LAI,KDIF,SINB,PARDIR,PARDIF
  COMPILE_OPT idl2
  XGAUSS = [0.1127017, 0.5000000, 0.8872983]                                        ;高斯点权重(Gauss point weights)
  WGAUSS = [0.2777778, 0.4444444, 0.2777778]
  SCV = 0.2
  REFH   = (1.0-SQRT(1.-SCV))/(1.+SQRT(1.-SCV))
  REFS   = REFH*2./(1.+1.6*SINB)
  KDIRBL = (0.5/SINB)*KDIF/(0.8*SQRT(1.-SCV))
  KDIRT  = KDIRBL*SQRT(1.0-SCV)
  FGROS  = 0.0
  FOR i2 = 0,2 DO BEGIN
    LAIC = LAI *XGAUSS[I2]
    VISDF  = (1.0-REFS)*PARDIF*KDIF*EXP(-KDIF *LAIC)
    VIST   = (1.0-REFS)*PARDIR*KDIRT *EXP(-KDIRT*LAIC)
    VISD   = (1.0-SCV)*PARDIR*KDIRBL*EXP(-KDIRBL*LAIC)
    VISSHD = VISDF+VIST-VISD
    MAX_AMAX1 = AMAX > 2.0
    FGRSH  = AMAX*(1.0-EXP(-VISSHD*EFF/MAX_AMAX1))
    ;        FGRSH  = AMAX*(1.0-EXP(-VISSHD*EFF/MAX([2.0,AMAX])))
    VISPP  = (1.0-SCV)*PARDIR/SINB
    
    FGRSUN = FGRSH *0.0
    index5=WHERE(VISPP EQ 0.0,count5)
    IF count5 GT 0 THEN BEGIN
      FGRSUN[index5] = FGRSH[index5]
    ENDIF
    index6=WHERE(VISPP NE 0.0,count6)
    IF count6 GT 0 THEN BEGIN
      FGRSUN[index6]=AMAX*(1.0-(AMAX-FGRSH[index6])*(1.0-EXP(-VISPP[index6]*EFF/MAX_AMAX1))/(EFF*VISPP[index6]))
    ENDIF
    
    
    ;        IF VISPP EQ 0.0 THEN BEGIN
    ;            FGRSUN = FGRSH
    ;        ENDIF ELSE BEGIN
    ;            FGRSUN=AMAX*(1.0-(AMAX-FGRSH)*(1.0-EXP(-VISPP*EFF/MAX_AMAX1))/(EFF*VISPP))
    ;        ENDELSE
    
    FSLLA  = EXP (-KDIRBL*LAIC)
    FGL= FSLLA*FGRSUN+(1.-FSLLA)*FGRSH
    FGROS  = FGROS+FGL*WGAUSS[I2]
  ENDFOR
  FGROS  = FGROS*LAI
  RETURN,FGROS
END

FUNCTION CAL_TOTASS,AMAX,EFF,LAI,KDIF,sunt,latitude,date,ju,NDVI_data,TEMP,AVRAD
  COMPILE_OPT idl2
  RAD=0.0174533
  PI= 3.1415926
  DTGA = 0.0
  Longtitude = 116.00
  XGAUSS = [0.1127017, 0.5000000, 0.8872983]                                                                                    
  WGAUSS = [0.2777778, 0.4444444, 0.2777778]
  Longtitude= FLOAT(Longtitude) * !PI / 180
  E_x = 1.0
  DAYL_data = CAL_DAYL(Ju,latitude)
  SCA = DAYL_data.SC_DATA
  DAYL = DAYL_data.DAYL_data_S
  Ju = julday(date.month, date.day, date.year) - julday(12, 31, date.year - 1)
  FOR i1 = 0,2 DO BEGIN
    hour = 12.0 +0.5*DAYL*XGAUSS[i1]
    
    TOTASS_data = CAL_ASTRO(Ju,latitude,AVRAD)
    SINLD = TOTASS_data.ASTRO_data2
    COSLD = TOTASS_data.ASTRO_data3
    SINBx = SINLD+COSLD*COS(2.*PI*(HOUR+12.)/24.)
    ZERO_DATA = SINBx*0.0
    sinb = SINBx > ZERO_DATA
    ;        sinb =   MAX([0,SINBx])                                                                                                    ;太阳高度角的计算(solar altitude)
    DSINBE = TOTASS_data.ASTRO_data4
    PAR = 0.5*AVRAD*sinb*(1.0+0.4*sinb)/DSINBE
    DIFPP = CAL_DIFPP(Ju,latitude,AVRAD,SCA)
    PAR_DIFFP = DIFPP*sinb
    PARDIF = PAR < PAR_DIFFP
    ;        PARDIF = MIN([PAR,PAR_DIFFP])
    PARDIR = PAR-PARDIF
    FGROS_data = CAL_ASSIM(AMAX,EFF,LAI,KDIF,SINB,PARDIR,PARDIF)
    DTGA = DTGA+FGROS_data*WGAUSS[I1]
  ENDFOR
  
  DTGA = DTGA*DAYL
  RETURN, DTGA
END

PRO CO2_assimilation,PGASS,AMAX,EFF,LAI,KDIF,sunt,latitude,date,ju,NDVI_data,TEMP,AVRAD
  COMPILE_OPT idl2
;  uu=DIALOG_MESSAGE('Co')
  DTGA = CAL_TOTASS(AMAX,EFF,LAI,KDIF,sunt,latitude,date,ju,NDVI_data,TEMP,AVRAD)                                                ;计算每日同化量
  PGASS = DTGA * 30.0/44.0
END