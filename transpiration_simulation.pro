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
;计算作物蒸腾，返回作物的蒸腾量给主工程模块(Caculate the crop evapotranspire and return the amount to the the main project, 
;corresponds to the EVTRA.FOR,SWEAF.FOR, modules of orginal WOFOST in fortran)
function CAl_SWEAF, ET0,CGNR
  SWEAF_a = 0.76
  SWEAF_b = 1.5
  SWEAF = 1./(SWEAF_a+SWEAF_b*ET0) - (5.-CGNR)*0.10
  if CGNR lt 3.0 then begin
    SWEAF = SWEAF + (ET0-0.6)/(CGNR*(CGNR+3.))
  endif
  lim_min = 0.1
  lim_max = 0.95
  lim_x = SWEAF
  SWEAF = CAL_LIMIT(lim_min,lim_max,lim_x)
  return, SWEAF
end

function CAL_EVTRA,IWB,IOX,IAIRDU,KDIF,CFET,DEPNR,E0,ES0,ET0,LAI,CRAIRC,SM,SM0,SMFCF,SMW
  DSOS = 0.0
  KGLOB = 0.75*KDIF
  ET0 = CFET*ET0
  EKL   = EXP(-KGLOB*LAI)
  EVWMX = E0*EKL
  EVSMX = 0.0 > (ES0*EKL)
  ;  EVSMX = MAX([0., ES0*EKL])
  TRAMX = 0.0001 > (ET0*(1.-EKL))
  ;  TRAMX = MAX([0.0001, ET0*(1.-EKL)])
  if IWB eq 0 then begin
    TRA  = TRAMX
    IDOS = 0
    IDWS = 0
  endif else begin
    ET0 = ET0
    CGNR = DEPNR
    SWDEP = CAL_SWEAF(ET0,CGNR)
    SMCR = (1.-SWDEP)*(SMFCF-SMW)+SMW
    lim_min = 0.0
    lim_max = 1.0
    lim_x = (SM-SMW)/(SMCR-SMW)
    RFWS = CAL_LIMIT(lim_min,lim_max,lim_x)
    if IAIRDU eq 0 and IOX eq 1 then begin
      SMAIR = SM0 - CRAIRC
      DSOS = SM *0.0
      index_tra1=WHERE(SM GE SMAIR ,count_tra1)
      IF count_tra1 GT 0 THEN BEGIN
        DSOS[index_tra1] = (DSOS[index_tra1]+1) < 4
      ENDIF
      index_tra2=WHERE(SM LT SMAIR ,count_tra2)
      IF count_tra2 GT 0 THEN BEGIN
        DSOS[index_tra2] = 0.0
      ENDIF
      
      ;      if SM ge SMAIR then DSOS = (DSOS+1) < 4
      ;      ;      if SM ge SMAIR then DSOS = min([(DSOS+1),4])
      ;      if SM lt SMAIR then DSOS = 0
      lim_min = 0.0
      lim_max = 1.0
      lim_x = (SM0-SM)/(SM0-SMAIR)
      RFOSMX = CAL_LIMIT(lim_min,lim_max,lim_x)
      RFOS   = RFOSMX + (1.-DSOS/4.)*(1.-RFOSMX)
    endif else begin
      if IAIRDU eq 1 and IOX eq 0 then begin
        RFOS = 1
      endif else begin
        RFOS = 1
      endelse
    endelse
    TRA = RFWS* RFOS * TRAMX
    
    IDOS = RFOS * 0.0
    IDWS = RFOS * 0.0
    
    index_tra3=WHERE(RFOS LT 1.0 ,count_tra3)
    IF count_tra3 GT 0 THEN BEGIN
      IDOS[index_tra3] = 1
    ENDIF
    
    index_tra4=WHERE(RFWS LT 1.0 ,count_tra4)
    IF count_tra4 GT 0 THEN BEGIN
      IDWS[index_tra4] = 1
    ENDIF
    
    
  ;    if RFOS lt 1 then begin
  ;      IDOS = 1
  ;    endif
  ;    if RFWS lt 1then begin
  ;      IDWS = 1
  ;    endif
  endelse
  ;  TRA_data = [TRA,TRAMX,EVWMX,EVSMX]
  
  TRA_data = {TRA_data1:TRA,TRA_data2:TRAMX,TRA_data3:EVWMX,TRA_data4:EVSMX}
  return, TRA_data
end
pro transpiration_simulation,I_STESS,TRA,TRAMX,IWB,IOX,IAIRDU,KDIF,CFET,DEPNR,E0,ES0,ET0,LAI,CRAIRC,SM,SM0,SMFCF,SMW,SM_t,RD,RDI,RDRSTB1,RDRSTB2
  SM_t  = SM
  RDI = RD
  CFET = 1.0
  TRA_data = CAl_EVTRA(IWB,IOX,IAIRDU,KDIF,CFET,DEPNR,E0,ES0,ET0,LAI,CRAIRC,SM,SM0,SMFCF,SMW)                                     ;计算实际蒸腾速率
  TRA = TRA_data.TRA_data1
  TRAMX = TRA_data.TRA_data2
  
  index_tra5=WHERE(TRA EQ TRAMX AND I_STESS GT 0.0 ,count_tra5)
  IF count_tra5 GT 0 THEN BEGIN
    TRA[index_tra5] = TRA[index_tra5] * 0.95
  ENDIF
  ;  if TRA eq TRAMX and I_STESS gt 0.0 then TRA = TRA *0.95
  TRA = TRA *0.90
end