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
;计算作物的播种时间并返回到主工程模块中(Caculate the sow time and return the result to the main project
;corresponds to the STDAY.FOR module of orginal WOFOST in fortran)
FUNCTION CAL_STDAY,SPASS,SPOSS,SPADS,SPODS,i_stday,stday_start,stday_end,ES0,RAIN,j_stday,ju_stday
  COMPILE_OPT idl2
  
  IF i_stday EQ 1.0 THEN Ju1 = stday_start
  IF i_stday EQ 0.0 THEN BEGIN
  
  j_stday_nn = j_stday

;FOR ju_stday = 134,136 DO BEGIN
  
    FOR ju_stday = stday_start,j_stday DO BEGIN
      
      CAPRFU1 = [-0.5,0.0,0.1,0.40,1.0]
      CAPRFU2 = [0.5,0.2,0.15,0.10,0.05]
      SPAC = SPADS
      SPOC = SPODS
      SPAC = SPASS
      SPOC = SPOSS
      DEFLIM = 0
      WEXC   = 2.0
      EVS    = 0.
      CAPRMX = 0.
      SEEP   = 0.
      IDFWOR = -99
      ILWPER = 0
      IF WEXC GE 0.5 THEN BEGIN
        CAPRMX = 0.
        EVS    = ES0
      ENDIF ELSE BEGIN
        AF1 = CAPRFU1
        AF2 = CAPRFU2
        AF3 = -WEXC
        SSA = CAL_AFGEN(AF1,AF2,AF3)
        EVS = MIN([ES0,CAPRMX+RAIN])
      ENDELSE
      EVS = MEAN(EVS)
      WEXC = MAX([-1.0, WEXC+RAIN-EVS])
    
      IF WEXC GT 0.0 THEN BEGIN
        SEEP = MIN([WEXC*SPAC+SPOC,WEXC])
        WEXC = WEXC-SEEP
      ENDIF
      IF WEXC LE DEFLIM THEN BEGIN
        ILWPER = ILWPER+1
      ENDIF ELSE BEGIN
        ILWPER = 0.0
      ENDELSE
      IF IDFWOR EQ -99 AND ILWPER GE 1 THEN IDFWOR = ju_stday
      IF ILWPER GE 3.0 THEN BEGIN
        IF ju_stday GT stday_end THEN Ju1 = stday_end
        IF ju_stday LT stday_start THEN Ju1 =stday_start
        IF ju_stday GE stday_start AND ju_stday LE stday_end THEN Ju1 =ju_stday
      ENDIF
;      IF ju_stday GE stday_start AND ju_stday LE stday_end THEN ju_stday = j_stday
      
    ENDFOR
  ENDIF
  IF ILWPER LT 3.0 THEN Ju1 = stday_start +(stday_end-stday_start)/2
  RETURN,Ju1
END

PRO sow_time ,meto_text,file_ndviserise,SPASS,SPOSS,SPADS,SPODS,i_stday,stday_start,stday_end,ES0,RAIN,j_stday,Ju,ANGSTA, ANGSTB,Ju1,NDVI_count,DELT,nl_begin,ns,nl
  COMPILE_OPT idl2
  IF i_stday EQ 1.0 THEN Ju1 = Ju1 + 3
  IF i_stday EQ 0.0 THEN BEGIN
;    Ju1 = 99
    FOR i_NDVI4 = 0, NDVI_count,DELT DO BEGIN
;      ndviserise = READ_TIFF(file_ndviserise[i_NDVI3], geotiff = geotiff)
;      NDVI_data1 = ndviserise
      basename = FILE_BASENAME(file_ndviserise[i_NDVI4])
      NDVI_sow = READ_TIFF(file_ndviserise[i_NDVI4] ,SUB_RECT=[0,nl_begin,ns,nl],geotiff = geotiff)
;      sow_string = STRSPLIT(basename,'_',/EXTRACT)
;      mark = STRMID(mark,0,4)   
;      year = STRMID(mark,0,4)
;      month = STRMID(mark,4,2)
;      day = STRMID(mark,6,2)
;      date = {year: year, month: month, day: day}
      
      year = STRMID(basename,8,4)
      month = STRMID(basename,12,2)
      day = STRMID(basename,14,2)
      date = {year: year, month: month, day: day}
      mark = STRMID(basename,8,8)
      
      j_stday = julday(month,day, year) - julday(12,31, year-1)
      IF j_stday LE stday_start THEN CONTINUE
      PRINT,j_stday
      OPENR,lun,meto_text,/get_lun
      lines = FILE_LINES(meto_text)
      data = DBLARR(9,Lines)
      READF,lun,data
      FOR i_meto = 0,lines-1 DO BEGIN
        date_meto = data[0,i_meto]
        a_meto =STRTRIM(STRING(FLOOR(date_meto)),2)
        IF a_meto EQ mark THEN BEGIN
          b_meto = i_meto
        ENDIF ELSE BEGIN
          CONTINUE
        ENDELSE
        FREE_LUN,lun
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
      latitude = 0.84304285
      Longtitude = 2.0245819
      ELEV = 370
      E_x = 1.0
      ju = julday(date.month, date.day, date.year) - julday(12, 31, date.year - 1)
      Declination  = 0.409 * SIN(2 * !PI * Ju / 365 - 1.405)
      wt = 2*!PI*(0.0667*(Longtitude-latitude))/24
      w_a = SIN(latitude)*SIN(Declination)
      w_b = COS(latitude)*COS(Declination)
      b_SunAngle = w_a + w_b * COS(wt)
      w_r = 1367*0.75*b_SunAngle
      E_f = 0.426-0.443*NDVI_sow
      w_rn =(1.0-E_f)*w_r*1.2+ E_x*5.6697*10.0^(-8)*TEMP - E_x*5.6697*10.0^(-8)*TEMP
      AVRAD =w_rn*sunt *3.6*1000
      CFET_DATA = CAL_PENMAN(Ju,latitude,AVRAD,ELEV,ANGSTA,ANGSTB,TMIN,TMAX,TEMP,VAPOUR,WIND)
      ET0 = CFET_DATA.ET3
      E0 = CFET_DATA.ET1
      ES0 = CFET_DATA.ET2
;       FOR ju_stday = stday_start,j_stday DO BEGIN
      Ju1 = CAL_STDAY(SPASS,SPOSS,SPADS,SPODS,i_stday,stday_start,stday_end,ES0,RAIN,j_stday,ju_stday)
;      print,ju1
;      ENDFOR
      IF Ju1 GT 99 THEN i_NDVI4 = NDVI_count
    ENDFOR
  ENDIF
END