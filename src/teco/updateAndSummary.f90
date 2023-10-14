! This module is used to summary the values of hourly, daily, monthly and yearly
module update_and_summary
    use datatypes
    implicit NONE
    real convert_g2kg, convert_h2s

    contains
    subroutine updateHourly(vegn, itime, iyear, iday, ihour)
        implicit none
        integer, intent(in) :: itime, iyear, iday, ihour
        type(vegn_tile_type), intent(inout) :: vegn
        if (do_out_hr) call updateOutVars(itime, 0, vegn, outVars_h, iyear, iday, ihour)
    end subroutine updateHourly

    subroutine updateDaily(vegn, itime, iyear, iday, ihour)
        implicit none
        integer, intent(in) :: itime, iyear, iday, ihour
        type(vegn_tile_type), intent(inout) :: vegn
        if (do_out_day) call updateOutVars(itime, 24, vegn, outVars_d, iyear, iday, ihour)
        ! write(*,*) outVars_d%cLeaf(itime), 24, itime, st%QC(1)
    end subroutine updateDaily

    subroutine updateMonthly(vegn, itime, hoursOfmonth, iyear, iday, ihour)
        implicit none
        integer, intent(in) :: itime, hoursOfmonth, iyear, iday, ihour
        type(vegn_tile_type), intent(inout) :: vegn
        if (do_out_mon) call updateOutVars(itime, hoursOfmonth, vegn, outVars_m, iyear, iday, ihour)
    end subroutine updateMonthly

    subroutine updateYearly(vegn, itime, hoursOfYear, iyear, iday, ihour)
        implicit none
        integer, intent(in) :: itime, hoursOfYear, iyear, iday, ihour
        type(vegn_tile_type), intent(inout) :: vegn
        if(do_out_yr) call updateOutVars(itime, hoursOfYear, vegn, outVars_y, iyear, iday, ihour)
    end subroutine updateYearly

    subroutine updateOutVars(itime, ntime, vegn, outvars, iyear, iday, ihour)
        implicit none
        integer, intent(in) :: itime, ntime, iyear, iday, ihour
        type(outvars_data_type), intent(inout) :: outVars
        type(vegn_tile_type), intent(in) :: vegn
        integer :: ipft, npft
        ! integer iTotHourly
        outVars%year(itime) = iyear
        outVars%doy(itime)  = iday
        outVars%hour(itime) = ihour
        ! stop
        convert_g2kg = 1 !0.001
        convert_h2s  = 1!1/3600.
        if (allocated(outvars%allSpec)) then
            npft = size(outvars%allSpec)
            do ipft = 1, npft
                ! carbon fluxes (Kg C m-2 s-1)
                outvars%allSpec(ipft)%gpp(itime)      = outvars%allSpec(ipft)%gpp(itime)     + &
                                                          vegn%allSp(ipft)%gpp*convert_g2kg*convert_h2s/ntime
                ! if (outvars%allSpec(ipft)%gpp(itime) > 100 .or. outvars%allSpec(ipft)%gpp(itime) < 0.) then
                !     print *, "test gpp: ", outvars%allSpec(ipft)%gpp(itime), vegn%allSp(ipft)%gpp*convert_g2kg*convert_h2s/ntime
                !     stop
                ! endif
                ! if (vegn%allSp(ipft)%gpp>0) print*, "gpp > 0",  outvars%allSpec(ipft)%gpp(itime)
                ! outvars%allSpec(ipft)%nee           = vegn%allSp(ipft)%NEE
                outvars%allSpec(ipft)%npp(itime)      = outvars%allSpec(ipft)%npp(itime)     + &
                                                          vegn%allSp(ipft)%npp*convert_g2kg*convert_h2s/ntime
                outvars%allSpec(ipft)%nppLeaf(itime)  = outvars%allSpec(ipft)%nppLeaf(itime) + &
                                                          vegn%allSp(ipft)%NPP_L*convert_g2kg*convert_h2s/ntime
                outvars%allSpec(ipft)%nppWood(itime)  = outvars%allSpec(ipft)%nppWood(itime) + &
                                                          vegn%allSp(ipft)%NPP_W*convert_g2kg*convert_h2s/ntime
                outvars%allSpec(ipft)%nppStem(itime)  = outvars%allSpec(ipft)%nppStem(itime) + &
                                                          vegn%allSp(ipft)%NPP_W*convert_g2kg*convert_h2s/ntime
                outvars%allSpec(ipft)%nppRoot(itime)  = outvars%allSpec(ipft)%nppRoot(itime) + &
                                                          vegn%allSp(ipft)%NPP_R*convert_g2kg*convert_h2s/ntime
                outvars%allSpec(ipft)%nppOther        = outvars%allSpec(ipft)%nppOther(itime) + &
                                                          vegn%allSp(ipft)%NSC*convert_g2kg*convert_h2s/ntime    ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
                outvars%allSpec(ipft)%ra(itime)       = outvars%allSpec(ipft)%ra(itime)      + &
                                                          vegn%allSp(ipft)%Rauto*convert_g2kg*convert_h2s/ntime
                outvars%allSpec(ipft)%raLeaf(itime)   = outvars%allSpec(ipft)%raLeaf(itime)  + &
                                                          vegn%allSp(ipft)%RmLeaf*convert_g2kg*convert_h2s/ntime
                outvars%allSpec(ipft)%raStem(itime)   = outvars%allSpec(ipft)%raStem(itime)  + &
                                                          vegn%allSp(ipft)%RmStem*convert_g2kg*convert_h2s/ntime
                outvars%allSpec(ipft)%raRoot(itime)   = outvars%allSpec(ipft)%raRoot(itime)  + &
                                                          vegn%allSp(ipft)%RmRoot*convert_g2kg*convert_h2s/ntime
                ! outvars%allSpec(ipft)%raOther  = vegn%allSp(ipft)%
                outvars%allSpec(ipft)%rMaint(itime)   = outvars%allSpec(ipft)%rMaint(itime)  + &
                                                          vegn%allSp(ipft)%Rmain*convert_g2kg*convert_h2s/ntime
                outvars%allSpec(ipft)%rGrowth(itime)  = outvars%allSpec(ipft)%rGrowth(itime) + &
                                                          vegn%allSp(ipft)%Rgrowth*convert_g2kg*convert_h2s/ntime
                ! outvars%allSpec(ipft)%nbp      = vegn%allSp(ipft)%
                ! Carbon Pools  (KgC m-2)
                outvars%allSpec(ipft)%cLeaf(itime)    = outvars%allSpec(ipft)%cLeaf(itime)  + &
                                                          vegn%allSp(ipft)%QC(1)*convert_g2kg/ntime
                outvars%allSpec(ipft)%cStem(itime)    = outvars%allSpec(ipft)%cStem(itime)  + &
                                                          vegn%allSp(ipft)%QC(2)*convert_g2kg/ntime
                outvars%allSpec(ipft)%cRoot(itime)    = outvars%allSpec(ipft)%cRoot(itime)  + &
                                                          vegn%allSp(ipft)%QC(3)*convert_g2kg/ntime
                ! Nitrogen pools (kgN m-2)
                outvars%allSpec(ipft)%nLeaf(itime)    = outvars%allSpec(ipft)%nLeaf(itime)  + &
                                                          vegn%allSp(ipft)%QN(1)*convert_g2kg/ntime
                outvars%allSpec(ipft)%nStem(itime)    = outvars%allSpec(ipft)%nStem(itime)  + &
                                                          vegn%allSp(ipft)%QN(2)*convert_g2kg/ntime
                outvars%allSpec(ipft)%nRoot(itime)    = outvars%allSpec(ipft)%nRoot(itime)  + &
                                                          vegn%allSp(ipft)%QN(3)*convert_g2kg/ntime
                ! water fluxes (kg m-2 s-1)
                outvars%allSpec(ipft)%tran(itime)     = outvars%allSpec(ipft)%tran(itime)   + &
                                                          vegn%allSp(ipft)%transp*convert_g2kg*convert_h2s/ntime
                ! other
                outvars%allSpec(ipft)%lai(itime)      = outvars%allSpec(ipft)%lai(itime)    + &
                                                          vegn%allSp(ipft)%lai/ntime
            enddo
        endif
        ! carbon fluxes (KgC m-2 s-1) Jian: TECO unit is gC m-2 h-1
        outvars%gpp(itime)             = outvars%gpp(itime)      + vegn%gpp*convert_g2kg*convert_h2s/ntime
        outvars%npp(itime)             = outvars%npp(itime)      + vegn%npp*convert_g2kg*convert_h2s/ntime
        outvars%nppLeaf(itime)         = outvars%nppLeaf(itime)  + vegn%NPP_L*convert_g2kg*convert_h2s/ntime
        outvars%nppWood(itime)         = outvars%nppWood(itime)  + vegn%NPP_W*convert_g2kg*convert_h2s/ntime  
        outvars%nppStem(itime)         = outvars%nppStem(itime)  + vegn%NPP_W*convert_g2kg*convert_h2s/ntime 
        outvars%nppRoot(itime)         = outvars%nppRoot(itime)  + vegn%NPP_R*convert_g2kg*convert_h2s/ntime
        outvars%nppOther(itime)        = outvars%nppOther(itime) + vegn%NSC*convert_g2kg*convert_h2s/ntime 
        outvars%ra(itime)              = outvars%ra(itime)       + vegn%Rauto*convert_g2kg*convert_h2s/ntime
        outvars%raLeaf(itime)          = outvars%raLeaf(itime)   + vegn%Rmleaf*convert_g2kg*convert_h2s/ntime
        outvars%raStem(itime)          = outvars%raStem(itime)   + vegn%Rmstem*convert_g2kg*convert_h2s/ntime
        outvars%raRoot(itime)          = outvars%raRoot(itime)   + vegn%Rmroot*convert_g2kg*convert_h2s/ntime
        outvars%raOther(itime)         = outvars%raOther(itime)  + st%Rnitrogen *convert_g2kg*convert_h2s/ntime
        outvars%rMaint(itime)          = outvars%rMaint(itime)   + vegn%Rmain *convert_g2kg*convert_h2s/ntime
        outvars%rGrowth(itime)         = outvars%rGrowth(itime)  + vegn%Rgrowth *convert_g2kg*convert_h2s/ntime 
        outvars%rh(itime)              = outvars%rh(itime)       + st%Rhetero *convert_g2kg*convert_h2s/ntime 
        outvars%nbp(itime)             = outvars%nbp(itime)      + &
                                           (vegn%gpp - st%Rhetero - vegn%Rauto) *convert_g2kg*convert_h2s/ntime   
        outvars%wetlandCH4(itime)      = outvars%wetlandCH4(itime) + st%simuCH4 *convert_g2kg*convert_h2s/ntime   
        outvars%wetlandCH4prod(itime)  = outvars%wetlandCH4prod(itime) + st%Pro_sum *convert_g2kg*convert_h2s/ntime 
        outvars%wetlandCH4cons(itime)  = outvars%wetlandCH4cons(itime) + st%Oxi_sum *convert_g2kg*convert_h2s/ntime 
        ! Carbon Pools  (KgC m-2)
        outvars%cLeaf(itime)           = outvars%cLeaf(itime) + st%QC(1)*convert_g2kg/ntime
        outvars%cStem(itime)           = outvars%cStem(itime) + st%QC(2)*convert_g2kg/ntime
        outvars%cRoot(itime)           = outvars%cRoot(itime) + st%QC(3)*convert_g2kg/ntime
        outvars%cOther(itime)          = outvars%cOther(itime)+ vegn%NSC*convert_g2kg/ntime
        outvars%cLitter(itime)         = outvars%cLitter(itime) + st%QC(4)*convert_g2kg/ntime
        outvars%cLitterCwd(itime)      = outvars%cLitterCwd(itime) + st%QC(5)*convert_g2kg/ntime
        outvars%cSoil(itime)           = outvars%cSoil(itime) + (st%QC(6) + st%QC(7) + st%QC(8))*convert_g2kg/ntime
        outvars%cSoilLevels(itime, :)  = outvars%cSoilLevels(itime, :) + (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)/ntime
        outvars%cSoilFast(itime)       = outvars%cSoilFast(itime) + st%QC(6)*convert_g2kg/ntime 
        outvars%cSoilSlow(itime)       = outvars%cSoilSlow(itime) + st%QC(7)*convert_g2kg/ntime 
        outvars%cSoilPassive(itime)    = outvars%cSoilPassive(itime) + st%QC(8)*convert_g2kg/ntime 
        outvars%CH4(itime, :)          = outvars%CH4(itime, :) + st%CH4*convert_g2kg/ntime 
        ! Nitrogen fluxes (kgN m-2 s-1)
        outvars%fBNF(itime)            = outvars%fBNF(itime) + st%N_fixation*convert_g2kg*convert_h2s/ntime 
        outvars%fN2O(itime)            = outvars%fN2O(itime) + &
                                           (st%N_transfer+st%N_uptake+st%N_fixation)*convert_g2kg*convert_h2s/ntime
        outvars%fNloss(itime)          = outvars%fNloss(itime) + &
                                            (vegn%N_leaf+vegn%N_wood+vegn%N_root)*convert_g2kg*convert_h2s/ntime
        outvars%fNnetmin(itime)        = outvars%fNnetmin(itime) + st%fNnetmin*convert_g2kg*convert_h2s/ntime 
        outvars%fNdep(itime)           = outvars%fNdep(itime) + st%N_deposit*convert_g2kg*convert_h2s/ntime 
        ! Nitrogen pools (kgN m-2)
        outvars%nLeaf(itime)           = outvars%nLeaf(itime) + st%QN(1)*convert_g2kg/ntime
        outvars%nStem(itime)           = outvars%nStem(itime) + st%QN(2)*convert_g2kg/ntime
        outvars%nRoot(itime)           = outvars%nRoot(itime) + st%QN(3)*convert_g2kg/ntime
        outvars%nOther(itime)          = outvars%nOther(itime) + vegn%NSN*convert_g2kg/ntime
        outvars%nLitter(itime)         = outvars%nLitter(itime) + st%QN(4)*convert_g2kg/ntime
        outvars%nLitterCwd(itime)      = outvars%nLitterCwd(itime) + st%QN(5)*convert_g2kg/ntime
        outvars%nSoil(itime)           = outvars%nSoil(itime) + (st%QN(6)+st%QN(7)+st%QN(8))*convert_g2kg/ntime
        outvars%nMineral(itime)        = outvars%nMineral(itime) + st%QNminer*convert_g2kg/ntime 
        ! energy fluxes (W m-2)
        outvars%hfls(itime)            = outvars%hfls(itime) + st%Hsoil/ntime ! Sensible heat flux;
        outvars%hfss(itime)            = outvars%hfss(itime) + st%Esoil/ntime ! Latent heat flux;
        outvars%SWnet(itime)           = outvars%SWnet(itime) + 0/ntime       ! Net shortwave radiation;
        outvars%LWnet(itime)           = outvars%LWnet(itime) + 0/ntime       ! Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        outvars%ec(itime)              = outvars%ec(itime)    + 0/ntime       !evap*convert_g2kg*convert_h2s/ntime        ! Canopy evaporation;
        outvars%tran(itime)            = outvars%tran(itime)  + vegn%transp*convert_g2kg*convert_h2s/ntime      ! Canopy transpiration;
        outvars%es(itime)              = outvars%es(itime)    + st%evap*convert_g2kg*convert_h2s/ntime ! Soil evaporation
        outvars%hfsbl(itime)           = outvars%hfsbl(itime) + st%sublim*convert_g2kg*convert_h2s/ntime ! Snow sublimation
        outvars%mrro(itime)            = outvars%mrro(itime)  + st%runoff*convert_g2kg*convert_h2s/ntime
        ! outvars%mrros(itime)         = forcing(iforcing)%Rain    
        outvars%mrrob(itime)           = outvars%mrrob(itime) + 0/ntime ! Total runoff; Surface runoff; Subsurface runoff
        ! other
        outvars%mrso(itime, :)         = outvars%mrso(itime, :) + st%liq_water*1000/ntime  ! Kg m-2, soil moisture in each soil layer
        outvars%tsl(itime,:)           = outvars%tsl(itime,:) + (st%tsoil_layer(1:10)+273.15)/ntime                            ! K, soil temperature in each soil layer Jian: not sure the tsoil_layer is correct or not
        ! outvars%tsland(itime)        = forcing(iforcing)%Tair+273.15                                   ! K, surface temperature
        outvars%wtd(itime)             = outvars%wtd(itime) +  (st%zwt/1000)/ntime                                       ! m, Water table depth
        outvars%snd(itime)             = outvars%snd(itime) +  (st%snow_depth/100)/ntime                               ! m, Total snow depth, Jian: change from m to cm in code, and now change from cm to m
        outvars%lai(itime)             = outvars%lai(itime) +  (vegn%LAI)/ntime                                           ! m2 m-2, Leaf area index
       
    end subroutine updateOutVars

    ! subroutine updateHourly(vegn, itime)
    !     implicit none
    !     type(vegn_tile_type), intent(in) :: vegn
    !     integer, intent(in) :: itime
    !     integer :: ipft, npft
    !     ! integer iTotHourly
    !     convert_g2kg = 0.001
    !     convert_h2s  = 1/3600.
    !     if (allocated(outVars_h%allSpec)) then
    !         npft = size(outVars_h%allSpec)
    !         do ipft = 1, npft
    !             ! carbon fluxes (Kg C m-2 s-1)
    !             outVars_h%allSpec(ipft)%gpp(itime)      = vegn%allSp(ipft)%gpp*convert_g2kg*convert_h2s
    !             ! outVars_h%allSpec(ipft)%nee      = vegn%allSp(ipft)%NEE
    !             outVars_h%allSpec(ipft)%npp(itime)      = vegn%allSp(ipft)%npp*convert_g2kg*convert_h2s
    !             outVars_h%allSpec(ipft)%nppLeaf(itime)  = vegn%allSp(ipft)%NPP_L*convert_g2kg*convert_h2s
    !             outVars_h%allSpec(ipft)%nppWood(itime)  = vegn%allSp(ipft)%NPP_W*convert_g2kg*convert_h2s
    !             outVars_h%allSpec(ipft)%nppStem(itime)  = vegn%allSp(ipft)%NPP_W*convert_g2kg*convert_h2s
    !             outVars_h%allSpec(ipft)%nppRoot(itime)  = vegn%allSp(ipft)%NPP_R*convert_g2kg*convert_h2s
    !             ! outVars_h%allSpec(ipft)%nppOther = vegn%allSp(ipft)%nppOther    ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
    !             outVars_h%allSpec(ipft)%ra(itime)       = vegn%allSp(ipft)%Rauto*convert_g2kg*convert_h2s
    !             outVars_h%allSpec(ipft)%raLeaf(itime)   = vegn%allSp(ipft)%RmLeaf*convert_g2kg*convert_h2s
    !             outVars_h%allSpec(ipft)%raStem(itime)   = vegn%allSp(ipft)%RmStem*convert_g2kg*convert_h2s
    !             outVars_h%allSpec(ipft)%raRoot(itime)   = vegn%allSp(ipft)%RmRoot*convert_g2kg*convert_h2s
    !             ! outVars_h%allSpec(ipft)%raOther  = vegn%allSp(ipft)%
    !             outVars_h%allSpec(ipft)%rMaint(itime)   = vegn%allSp(ipft)%Rmain*convert_g2kg*convert_h2s
    !             outVars_h%allSpec(ipft)%rGrowth(itime)  = vegn%allSp(ipft)%Rgrowth*convert_g2kg*convert_h2s
    !             ! outVars_h%allSpec(ipft)%nbp      = vegn%allSp(ipft)%
    !             ! Carbon Pools  (KgC m-2)
    !             outVars_h%allSpec(ipft)%cLeaf(itime)    = vegn%allSp(ipft)%QC(1)*convert_g2kg
    !             outVars_h%allSpec(ipft)%cStem(itime)    = vegn%allSp(ipft)%QC(2)*convert_g2kg
    !             outVars_h%allSpec(ipft)%cRoot(itime)    = vegn%allSp(ipft)%QC(3)*convert_g2kg
    !             ! Nitrogen pools (kgN m-2)
    !             outVars_h%allSpec(ipft)%nLeaf(itime)    = vegn%allSp(ipft)%QN(1)*convert_g2kg
    !             outVars_h%allSpec(ipft)%nStem(itime)    = vegn%allSp(ipft)%QN(2)*convert_g2kg
    !             outVars_h%allSpec(ipft)%nRoot(itime)    = vegn%allSp(ipft)%QN(3)*convert_g2kg
    !             ! water fluxes (kg m-2 s-1)
    !             outVars_h%allSpec(ipft)%tran(itime)     = vegn%allSp(ipft)%transp*convert_g2kg*convert_h2s
    !             ! other
    !             outVars_h%allSpec(ipft)%lai(itime)      = vegn%allSp(ipft)%LAI
    !         enddo
    !     endif
    !     ! carbon fluxes (KgC m-2 s-1) Jian: TECO unit is gC m-2 h-1
    !     outVars_h%gpp(itime)             = vegn%gpp*convert_g2kg*convert_h2s
    !     outVars_h%npp(itime)             = vegn%npp*convert_g2kg*convert_h2s
    !     outVars_h%nppLeaf(itime)         = vegn%NPP_L*convert_g2kg*convert_h2s
    !     outVars_h%nppWood(itime)         = vegn%NPP_W*convert_g2kg*convert_h2s  
    !     outVars_h%nppStem(itime)         = vegn%NPP_W*convert_g2kg*convert_h2s                   ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues. Jian: TECO has no above ground woody tissues, set to equit wood
    !     outVars_h%nppRoot(itime)         = vegn%NPP_R*convert_g2kg*convert_h2s
    !     outVars_h%nppOther(itime)        = 0*convert_g2kg*convert_h2s                       ! Jian: no other storage, NSC seems different from other NPP.
    !     outVars_h%ra(itime)              = vegn%Rauto*convert_g2kg*convert_h2s
    !     outVars_h%raLeaf(itime)          = vegn%Rmleaf*convert_g2kg*convert_h2s
    !     outVars_h%raStem(itime)          = vegn%Rmstem*convert_g2kg*convert_h2s
    !     outVars_h%raRoot(itime)          = vegn%Rmroot*convert_g2kg*convert_h2s
    !     outVars_h%raOther(itime)         = st%Rnitrogen *convert_g2kg*convert_h2s               ! Total C cost for nitrogen
    !     outVars_h%rMaint(itime)          = vegn%Rmain *convert_g2kg*convert_h2s                   ! maintenance respiration
    !     outVars_h%rGrowth(itime)         = vegn%Rgrowth *convert_g2kg*convert_h2s                 ! growth respiration
    !     outVars_h%rh(itime)              = st%Rhetero *convert_g2kg*convert_h2s                 ! heterotrophic respiration
    !     outVars_h%nbp(itime)             = (vegn%gpp - st%Rhetero - vegn%Rauto) *convert_g2kg*convert_h2s   ! NBP(net biome productivity) = GPP - Rh - Ra - other losses  
    !     outVars_h%wetlandCH4(itime)      = st%simuCH4 *convert_g2kg*convert_h2s                 ! wetland net fluxes of CH4
    !     outVars_h%wetlandCH4prod(itime)  = st%Pro_sum *convert_g2kg*convert_h2s                 ! wetland net fluxes of CH4 production
    !     outVars_h%wetlandCH4cons(itime)  = st%Oxi_sum *convert_g2kg*convert_h2s                 ! wetland net fluxes of CH4 consumption
    !     ! Carbon Pools  (KgC m-2)
    !     outVars_h%cLeaf(itime)           = st%QC(1)*convert_g2kg
    !     outVars_h%cStem(itime)           = st%QC(2)*convert_g2kg
    !     outVars_h%cRoot(itime)           = st%QC(3)*convert_g2kg
    !     outVars_h%cOther(itime)          = vegn%NSC*convert_g2kg                        ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
    !     outVars_h%cLitter(itime)         = st%QC(4)*convert_g2kg                      ! litter (excluding coarse woody debris), Jian: fine litter in TECO?
    !     outVars_h%cLitterCwd(itime)      = st%QC(5)*convert_g2kg                      ! cLitterCwd: carbon in coarse woody debris
    !     outVars_h%cSoil(itime)           = (st%QC(6) + st%QC(7) + st%QC(8))*convert_g2kg    ! cSoil: soil organic carbon (Jian: total soil carbon);
    !     outVars_h%cSoilLevels(itime, :)     = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)                                       ! cSoilLevels(depth-specific soil organic carbon, Jian: depth?);
    !     outVars_h%cSoilFast(itime)       = st%QC(6)*convert_g2kg                      ! cSoilPools (different pools without depth)
    !     outVars_h%cSoilSlow(itime)       = st%QC(7)*convert_g2kg 
    !     outVars_h%cSoilPassive(itime)    = st%QC(8)*convert_g2kg 
    !     outVars_h%CH4(itime, :)          = st%CH4*convert_g2kg                        ! methane concentration
    !     ! Nitrogen fluxes (kgN m-2 s-1)
    !     outVars_h%fBNF(itime)            = st%N_fixation*convert_g2kg*convert_h2s                 ! fBNF: biological nitrogen fixation;
    !     outVars_h%fN2O(itime)            = (st%N_transfer+st%N_uptake+st%N_fixation)*convert_g2kg*convert_h2s                    ! fN2O: loss of nitrogen through emission of N2O;
    !     outVars_h%fNloss(itime)          = (vegn%N_leaf+vegn%N_wood+vegn%N_root)*convert_g2kg*convert_h2s                     ! fNloss:Total loss of nitrogen to the atmosphere and from leaching;
    !     outVars_h%fNnetmin(itime)        = st%fNnetmin*convert_g2kg*convert_h2s !((N_transfer+N_uptake+N_fixation)-(N_leaf+N_wood+N_root))*convert_g2kg*convert_h2s                   ! net mineralizaiton
    !     outVars_h%fNdep(itime)           = st%N_deposit*convert_g2kg*convert_h2s                  ! deposition of N
    !     ! Nitrogen pools (kgN m-2)
    !     outVars_h%nLeaf(itime)           = st%QN(1)*convert_g2kg
    !     outVars_h%nStem(itime)           = st%QN(2)*convert_g2kg
    !     outVars_h%nRoot(itime)           = st%QN(3)*convert_g2kg
    !     outVars_h%nOther(itime)          = vegn%NSN*convert_g2kg         ! other N pool
    !     outVars_h%nLitter(itime)         = st%QN(4)*convert_g2kg
    !     outVars_h%nLitterCwd(itime)      = st%QN(5)*convert_g2kg
    !     outVars_h%nSoil(itime)           = (st%QN(6)+st%QN(7)+st%QN(8))*convert_g2kg
    !     outVars_h%nMineral(itime)        = st%QNminer*convert_g2kg                                 ! nMineral: Mineral nitrogen pool
    !     ! energy fluxes (W m-2)
    !     outVars_h%hfls(itime)            = st%Hsoil ! Sensible heat flux;
    !     outVars_h%hfss(itime)            = st%Esoil ! Latent heat flux;
    !     outVars_h%SWnet(itime)           = 0 ! Net shortwave radiation;
    !     outVars_h%LWnet(itime)           = 0 ! Net longwave radiation
    !     ! water fluxes (kg m-2 s-1)
    !     outVars_h%ec(itime)              = 0!evap*convert_g2kg*convert_h2s        ! Canopy evaporation;
    !     outVars_h%tran(itime)            = vegn%transp*convert_g2kg*convert_h2s      ! Canopy transpiration;
    !     outVars_h%es(itime)              = st%evap*convert_g2kg*convert_h2s ! Soil evaporation
    !     outVars_h%hfsbl(itime)           = st%sublim*convert_g2kg*convert_h2s ! Snow sublimation
    !     outVars_h%mrro(itime)            = st%runoff*convert_g2kg*convert_h2s
    !     ! outVars_h%mrros(itime)           = forcing(iforcing)%Rain    
    !     outVars_h%mrrob(itime)           = 0 ! Total runoff; Surface runoff; Subsurface runoff
    !     ! other
    !     outVars_h%mrso(itime, :)         = st%liq_water*1000                                   ! Kg m-2, soil moisture in each soil layer
    !     outVars_h%tsl(itime,:)             = st%tsoil_layer(1:10)+273.15                            ! K, soil temperature in each soil layer Jian: not sure the tsoil_layer is correct or not
    !     ! outVars_h%tsland(itime)          = forcing(iforcing)%Tair+273.15                                   ! K, surface temperature
    !     outVars_h%wtd(itime)             = st%zwt/1000                                       ! m, Water table depth
    !     outVars_h%snd(itime)             = st%snow_depth/100                                ! m, Total snow depth, Jian: change from m to cm in code, and now change from cm to m
    !     outVars_h%lai(itime)             = vegn%LAI                                           ! m2 m-2, Leaf area index
        
    ! end subroutine updateHourly
end module update_and_summary