!#define USE_NETCDF

module io_mod
    use datatypes
#ifdef USE_NETCDF
        use netcdf
#endif
    implicit none
    CHARACTER(len=4) :: str_startyr, str_endyr
    
    contains

    subroutine write_outputs_csv(out_path, outVars, nSimuLen, str_freq)
        ! write the outputs to csv-format file
        implicit none
        character(*), intent(in) :: out_path, str_freq
        type(outvars_data_type), intent(in) :: outVars
        integer, intent(in) :: nSimuLen
        integer :: ipft
        ! ---------------------------------------------
        character(256) :: output_file
        integer :: unit, i, isimulen, nformat
        character(len=:), allocatable :: csv_fileName
        character(2000) :: header_csv
        character(len=2500) :: format_string
        ! print*,"test0..."
        allocate(character(len=200+len(out_path)) :: csv_fileName)

        csv_fileName = adjustl(trim(out_path))//"/simulation_"//str_freq//"_TECO-SPRUCE_"//&
            & adjustl(trim(case_name))//".csv" 
#if defined(WIN32) || defined(WIN64) || defined(_WIN32) || defined(_WIN64)
        csv_fileName = adjustl(trim(out_path))//"\simulation_"//str_freq//"_TECO-SPRUCE_"//&
            & adjustl(trim(case_name))//".csv"
#endif
        ! Write header line
        ! write(unit, *) 'Name', ',', 'Age'
        header_csv = "year,doy,hour,"
        do ipft = 1, count_pft
            header_csv = adjustl(trim(header_csv))//"gpp_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"nee_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"npp_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"nppLeaf_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"nppWood_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"nppStem_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"nppRoot_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"nppOther_"//adjustl(trim(spec_names(ipft)))//","    ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
            header_csv = adjustl(trim(header_csv))//"ra_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"raLeaf_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"raStem_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"raRoot_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"raOther_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"rMaint_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"rGrowth_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"nbp_"//adjustl(trim(spec_names(ipft)))//","
            ! Carbon Pools  (KgC m-2)
            header_csv = adjustl(trim(header_csv))//"cLeaf_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"cStem_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"cRoot_"//adjustl(trim(spec_names(ipft)))//","
            ! Nitrogen pools (kgN m-2)
            header_csv = adjustl(trim(header_csv))//"nLeaf_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"nStem_"//adjustl(trim(spec_names(ipft)))//","
            header_csv = adjustl(trim(header_csv))//"nRoot_"//adjustl(trim(spec_names(ipft)))//","
            ! water fluxes (kg m-2 s-1)
            header_csv = adjustl(trim(header_csv))//"tran_"//adjustl(trim(spec_names(ipft)))//","
            ! other
            header_csv = adjustl(trim(header_csv))//"lai_"//adjustl(trim(spec_names(ipft)))//"," 
        enddo
        header_csv = adjustl(trim(header_csv))//"gpp,nee,npp,nppLeaf,nppWood,nppStem,nppRoot,nppOther,ra,&
            raLeaf,raStem,raRoot,raOther,rMaint,rGrowth,rh,nbp,wetlandCH4,wetlandCH4prod,&
            wetlandCH4cons,cLeaf,cStem,cRoot,cOther,cLitter,cLitterCwd,cSoil,&
            cSoilLevels_1,cSoilLevels_2,cSoilLevels_3,cSoilLevels_4,cSoilLevels_5,&
            cSoilLevels_6,cSoilLevels_7,cSoilLevels_8,cSoilLevels_9,cSoilLevels_10,&
            cSoilFast,cSoilSlow,cSoilPassive,CH4_1,CH4_2,CH4_3,CH4_4,CH4_5,CH4_6,CH4_7,&
            CH4_8,CH4_9,CH4_10,fBNF,fN2O,fNloss,fNnetmin,fNdep,nLeaf,nStem,nRoot,nOther,&
            nLitter,nLitterCwd,nSoil,nMineral,hfls,hfss,SWnet,LWnet,ec,tran,es,hfsbl,&
            mrro,mrros,mrrob,mrso_1,mrso_2,mrso_3,mrso_4,mrso_5,mrso_6,mrso_7,mrso_8,&
            mrso_9,mrso_10,tsl_1,tsl_2,tsl_3,tsl_4,tsl_5,tsl_6,tsl_7,tsl_8,tsl_9,tsl_10,&
            &tsland,wtd,snd,lai"
        ! print*, header_csv
        ! Open the file for writing
        open(newunit=unit, file=csv_fileName, status='replace', action='write', iostat=i)
        write(unit, *) adjustl(trim(header_csv))
        ! write the date
        nformat = 24*count_pft+98 
        ! print*,"test1..."
        format_string = '((i4,",")(i3,",")(i2,",")' // repeat('(f15.4, ",")', nformat-1) // 'f15.4)'
        ! print*,format_string
        ! print*, outVars%allSpec(1)%gpp
        do i = 1, nSimuLen
            write(unit, adjustl(trim(format_string)))outVars%year(i), outVars%doy(i), outVars%hour(i), &
               (outVars%allSpec(ipft)%gpp(i),      outVars%allSpec(ipft)%nee(i),      outVars%allSpec(ipft)%npp(i),     &     
                outVars%allSpec(ipft)%nppLeaf(i),  outVars%allSpec(ipft)%nppWood(i),  outVars%allSpec(ipft)%nppStem(i), &
                outVars%allSpec(ipft)%nppRoot(i),  outVars%allSpec(ipft)%nppOther(i), outVars%allSpec(ipft)%ra(i),      & 
                outVars%allSpec(ipft)%raLeaf(i),   outVars%allSpec(ipft)%raStem(i),   outVars%allSpec(ipft)%raRoot(i),  & 
                outVars%allSpec(ipft)%raOther(i),  outVars%allSpec(ipft)%rMaint(i),   outVars%allSpec(ipft)%rGrowth(i), &
                outVars%allSpec(ipft)%nbp(i),      outVars%allSpec(ipft)%cLeaf(i),    outVars%allSpec(ipft)%cStem(i),   & 
                outVars%allSpec(ipft)%cRoot(i),    outVars%allSpec(ipft)%nLeaf(i),    outVars%allSpec(ipft)%nStem(i),   &  
                outVars%allSpec(ipft)%nRoot(i),    outVars%allSpec(ipft)%tran(i),     outVars%allSpec(ipft)%lai(i),     &    
                ipft = 1, count_pft),& 
                outVars%gpp(i),     outVars%nee(i),        outVars%npp(i),            outVars%nppLeaf(i),  &        
                outVars%nppWood(i), outVars%nppStem(i),    outVars%nppRoot(i),        outVars%nppOther(i), &   
                outVars%ra(i),      outVars%raLeaf(i),     outVars%raStem(i),         outVars%raRoot(i),   &  
                outVars%raOther(i), outVars%rMaint(i),     outVars%rGrowth(i),        outVars%rh(i),       &
                outVars%nbp(i),&     
                outVars%wetlandCH4(i), outVars%wetlandCH4prod(i), outVars%wetlandCH4cons(i),   &
                outVars%cLeaf(i),   outVars%cStem(i),      outVars%cRoot(i),          outVars%cOther(i),   &
                outVars%cLitter(i), outVars%cLitterCwd(i), outVars%cSoil(i),                  &
                outVars%cSoilLevels(i,1), outVars%cSoilLevels(i,2), outVars%cSoilLevels(i,3), & 
                outVars%cSoilLevels(i,4), outVars%cSoilLevels(i,5), outVars%cSoilLevels(i,6), &
                outVars%cSoilLevels(i,7), outVars%cSoilLevels(i,8), outVars%cSoilLevels(i,9), & 
                outVars%cSoilLevels(i,10),                                                    &
                outVars%cSoilFast(i),  outVars%cSoilSlow(i), outVars%cSoilPassive(i),            &
                outVars%CH4(i,1),      outVars%CH4(i,2),     outVars%CH4(i,3), outVars%CH4(i,4), &
                outVars%CH4(i,5),      outVars%CH4(i,6),     outVars%CH4(i,7), outVars%CH4(i,8), &
                outVars%CH4(i,9),      outVars%CH4(i,10), &
                outVars%fBNF(i),        outVars%fN2O(i),   outVars%fNloss(i),   outVars%fNnetmin(i),   outVars%fNdep(i),   &
                outVars%nLeaf(i),      outVars%nStem(i),  outVars%nRoot(i),    outVars%nOther(i),     outVars%nLitter(i), &
                outVars%nLitterCwd(i), outVars%nSoil(i),  outVars%nMineral(i), outVars%hfls(i),       outVars%hfss(i),    &
                outVars%SWnet(i),      outVars%LWnet(i),  outVars%ec(i),       outVars%tran(i),       outVars%es(i),      &
                outVars%hfsbl(i),      outVars%mrro(i),   outVars%mrros(i),    outVars%mrrob(i),      outVars%mrso(i,1),  &
                outVars%mrso(i,2),     outVars%mrso(i,3), outVars%mrso(i,4),   outVars%mrso(i,5),     outVars%mrso(i,6),  &
                outVars%mrso(i,7),     outVars%mrso(i,8), outVars%mrso(i,9),   outVars%mrso(i,10), &
                outVars%tsl(i,1),      outVars%tsl(i,2),  outVars%tsl(i,3),    outVars%tsl(i,4),      outVars%tsl(i,5), &
                outVars%tsl(i,6),      outVars%tsl(i,7),  outVars%tsl(i,8),    outVars%tsl(i,9),      outVars%tsl(i,10),&
                outVars%tsland(i),     outVars%wtd(i),    outVars%snd(i),      outVars%lai(i)
        enddo
        
! 83      format((i4),",",(i3),",",(i2),",",(f15.4,","))
        ! ! Write data lines
        ! do i = 1, 3
        !     write(unit, '(A,I0,A,I0)') 'Person', i, ',', 25 + i*5
        ! end do

        ! ! Close the file
        close(unit)
        deallocate(csv_fileName)
    end subroutine write_outputs_csv

#ifdef USE_NETCDF
    subroutine write_outputs_nc(out_path, outVars, nSimuLen, str_freq)
        ! Daily and monthly
        ! carbon flux (KgC m-2 s-1): gpp, npp, nppLeaf, nppWood, nppRoot, nppOther,
        !              ra, raLeaf, raStem, raRoot, raOther, rMaint, rGrowth, rh
        !              nbp (=gpp - Rh - Ra - other losses)
        !              wetlandCH4, wetlandCH4prod, wetlandCH4cons
        ! carbon pools (KgC m-2): cLeaf, cStem, cRoot, cOther, cLitter (excluding coarse wood debris), cLitterCwd
        !              cSoil, cSoilLevels, cSoilPools (soil organic carbon for each pool), CH4 (Methane concentration)
        ! Nitrogen flux (KgN m-2 s-1) : fBNF(biological nitrogen fixation), fN2O, fNloss, fNnetmin, fNdep
        ! Nitrogen pools (KgN m-2): nleaf, nStem, nRoot, nOther, nLitter, nLitterCwd, nSoil, nMineral
        ! Energy Fluxes (W m-2): hfls(sensible heat flux), hfss(Latent heat flux), SWnet (Net Shortwave radiation), LWnet(Net Longwave radiation)
        ! Water Fluxes  (Kg m-2 s-1): ec(canopy evaporation), tran(canopy transpiration), es(soil evaporation), hfsbl (snow sublimation), mrro(total runoff),
        !                 mrros (surface runoff), mrrob(subsurface runoff)
        ! other         : mrso (soil moisture in each soil layer, Kg m-2), tsl(soil temperature in each soil layer, K), tsland(surface temperature, K),
        !                 wtd (Water table depth, m), snd (total snow depth, m), lai(m2 m-2) 
        ! ===================================================================================================================================================
        ! carbon fluxes variables
        ! ----------:-----------:----------:-----------------------
        implicit none
        character(*), intent(in) :: out_path, str_freq
        type(outvars_data_type), intent(in) :: outVars
        integer, intent(in) :: nSimuLen
        integer :: ipft

        write(str_startyr,"(I4)")forcing(1)%year
        write(str_endyr,"(I4)")forcing(nforcing)%year

        if (allocated(outVars%allSpec)) then
            do ipft = 1, count_pft
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%gpp,      "gpp_"//adjustl(trim(spec_names(ipft))),     &
                    "kgC m-2 s-1", "gross primary productivity",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%nee,      "nee_"//adjustl(trim(spec_names(ipft))),      &
                    "kgC m-2 s-1", "net ecosystem exchange",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%npp,      "npp_"//adjustl(trim(spec_names(ipft))),      &
                    "kgC m-2 s-1", "net primary productivity",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%nppLeaf,  "nppLeaf_"//adjustl(trim(spec_names(ipft))),  &
                    "kgC m-2 s-1", "NPP allocated to leaf tissues",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%nppWood,  "nppWood_"//adjustl(trim(spec_names(ipft))),  &
                    "kgC m-2 s-1", "NPP allocated to above ground woody tissues",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%nppStem,  "nppStem_"//adjustl(trim(spec_names(ipft))),  &
                    "kgC m-2 s-1", "NPP allocated to stem tissues",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%nppRoot,  "nppRoot_"//adjustl(trim(spec_names(ipft))),  &
                    "kgC m-2 s-1", "NPP allocated to root tissues",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%nppOther, "nppOther_"//adjustl(trim(spec_names(ipft))), &
                    "kgC m-2 s-1", "NPP allocated to other plant organs (reserves, fruits, exudates)",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%ra, "ra_"//adjustl(trim(spec_names(ipft))), &
                    "kgC m-2 s-1", "Plant Autotrophic Respiration",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%raLeaf,   "raLeaf_"//adjustl(trim(spec_names(ipft))),   &
                    "kgC m-2 s-1", "Ra from leaves",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%raStem,   "raStem_"//adjustl(trim(spec_names(ipft))),   &
                    "kgC m-2 s-1", "Ra from above ground woody tissues",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%raRoot,   "raRoot_"//adjustl(trim(spec_names(ipft))),   &
                    "kgC m-2 s-1", "Ra from fine roots",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%raOther,   "raOther_"//adjustl(trim(spec_names(ipft))), &
                    "kgC m-2 s-1", "Ra from other plant organs (reserves, fruits, exudates)",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%rMaint,    "rMaint_"//adjustl(trim(spec_names(ipft))),  &
                    "kgC m-2 s-1", "Maintenance respiration",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%rGrowth,   "rGrowth_"//adjustl(trim(spec_names(ipft))), &
                    "kgC m-2 s-1", "Growth respiration",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%nbp,       "nbp_"//adjustl(trim(spec_names(ipft))),     &
                    "kgC m-2 s-1", "Net Biome productivity (NBP = GPP - Rh - Ra - other losses)",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%cLeaf,     "cLeaf_"//adjustl(trim(spec_names(ipft))),   &
                    "kgC m-2", "Carbon biomass in leaves",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%cStem,     "cStem_"//adjustl(trim(spec_names(ipft))),   &
                    "kgC m-2", "Carbon above ground woody biomass",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%cRoot,     "cRoot_"//adjustl(trim(spec_names(ipft))),   &
                    "kgC m-2", "Carbon biomass in roots",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%nLeaf,     "nLeaf_"//adjustl(trim(spec_names(ipft))),   &
                    "kgN m-2", "Nitrogen in leaves",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%nStem,     "nStem_"//adjustl(trim(spec_names(ipft))),   &
                    "kgN m-2", "Nitrogen in stems",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%nRoot,     "nRoot_"//adjustl(trim(spec_names(ipft))),   &
                    "kgN m-2", "Nirogen in roots",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%tran,      "tran_"//adjustl(trim(spec_names(ipft))),    &
                    "kg m-2 s-1", "Canopy transpiration",str_freq,1)
                call write_nc(out_path, nSimuLen, outVars%allSpec(ipft)%lai,       "lai_"//adjustl(trim(spec_names(ipft))),     &
                    "m2 m-2", "Leaf area index",str_freq,1)
            enddo
        endif

        ! outputs
        call write_nc(out_path, nSimuLen, outVars%gpp,"gpp","kgC m-2 s-1", "gross primary productivity",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%npp,"npp","kgC m-2 s-1", "Total net primary productivity",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%nppLeaf,"nppLeaf","kgC m-2 s-1", "NPP allocated to leaf tissues",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%nppWood,"nppWood","kgC m-2 s-1", &
            & "NPP allocated to above ground woody tissues",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%nppStem,"nppStem","kgC m-2 s-1", "NPP allocated to stem tissues",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%nppRoot,"nppRoot","kgC m-2 s-1", "NPP allocated to root tissues",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%nppOther,"nppOther","kgC m-2 s-1", &
            & "NPP allocated to other plant organs (reserves, fruits, exudates)",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%ra,"ra","kgC m-2 s-1", "Plant Autotrophic Respiration",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%raLeaf,"raLeaf","kgC m-2 s-1", "Ra from leaves",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%raStem,"raStem","kgC m-2 s-1", "Ra from above ground woody tissues",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%raRoot,"raRoot","kgC m-2 s-1", "Ra from fine roots",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%raOther,"raOther","kgC m-2 s-1", &
            & "Ra from other plant organs (reserves, fruits, exudates)",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%rMaint,"rMaint","kgC m-2 s-1", "Maintenance respiration",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%rGrowth,"rGrowth","kgC m-2 s-1", "Growth respiration",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%rh,"rh","kgC m-2 s-1", "Heterotrophic respiration rate",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%nbp,"nbp","kgC m-2 s-1", &
            &"Net Biome productivity (NBP = GPP - Rh - Ra - other losses)",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%wetlandCH4,"wetlandCH4","kgC m-2 s-1", "Net fluxes of CH4",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%wetlandCH4prod,"wetlandCH4prod","kgC m-2 s-1", "CH4 production",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%wetlandCH4cons,"wetlandCH4cons","kgC m-2 s-1", "CH4 consumption",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%cLeaf,"cLeaf","kgC m-2", "Carbon biomass in leaves",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%cStem,"cStem","kgC m-2", "Carbon above ground woody biomass",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%cRoot,"cRoot","kgC m-2", "Carbon biomass in roots",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%cOther,"cOther","kgC m-2", &
            & "Carbon biomass in other plant organs (reserves, fruits)",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%cLitter,"cLitter","kgC m-2", &
            & "Carbon in litter (excluding coarse woody debris)",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%cLitterCwd,"cLitterCwd","kgC m-2", "Carbon in coarse woody debris",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%cSoil,"cSoil","kgC m-2", "Total soil organic carbon",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%cSoilLevels,"cSoilLevels","kgC m-2", &
            & "Depth-specific soil organic carbon",str_freq,nlayers)
        call write_nc(out_path, nSimuLen, outVars%cSoilFast,"cSoilFast","kgC m-2", "Fast soil organic carbon",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%cSoilSlow,"cSoilSlow","kgC m-2 s-1", "Slow soil organic carbon",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%cSoilPassive,"cSoilPassive","kgC m-2 s-1", &
            & "Passive soil organic carbon",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%CH4,"CH4","kgC m-2 s-1", "Methane concentration",str_freq,nlayers)
        call write_nc(out_path, nSimuLen, outVars%fBNF,"fBNF","kgN m-2 s-1", "biological nitrogen fixation",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%fN2O,"fN2O","kgN m-2 s-1", &
            & "loss of nitrogen through emission of N2O",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%fNloss,"fNloss","kgN m-2 s-1", &
            & "Total loss of nitrogen to the atmosphere and from leaching",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%fNnetmin,"fNnetmin","kgN m-2 s-1", "net mineralization of N",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%fNdep,"fNdep","kgN m-2 s-1", "Nitrogen deposition",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%nLeaf,"nLeaf","kgN m-2", "Nitrogen in leaves",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%nStem,"nStem","kgN m-2", "Nitrogen in stems",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%nRoot,"nRoot","kgN m-2", "Nirogen in roots",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%nOther,     "nOther","kgN m-2", &
            & "nitrogen in other plant organs (reserves, fruits)",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%nLitter,    "nLitter","kgN m-2", &
            & "Nitrogen in litter (excluding coarse woody debris)",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%nLitterCwd, "nLitterCwd","kgN m-2", &
            & "Nitrogen in coarse woody debris",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%nSoil,"nSoil","kgN m-2", "Nitrogen in soil organic matter",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%nMineral,"nMineral","kgN m-2", "Mineral nitrogen pool",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%hfls,"hfls","W m-2", "Sensible heat flux",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%hfss,"hfss","W m-2", "Latent heat flux",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%SWnet,"SWnet","W m-2", "Net shortwave radiation",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%LWnet,"LWnet","W m-2", "Net longwave radiation",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%ec,"ec","kg m-2 s-1", "Canopy evaporation",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%tran,"tran","kg m-2 s-1", "Canopy transpiration",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%es,"es","kg m-2 s-1", "Soil evaporation",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%hfsbl,"hfsbl","kg m-2 s-1", "Snow sublimation",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%mrro,"mrro","kg m-2 s-1", "Total runoff",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%mrros,"mrros","kg m-2 s-1", "Surface runoff",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%mrrob,"mrrob","kg m-2 s-1", "Subsurface runoff",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%mrso,"mrso","kg m-2", "soil moisture in each soil layer",str_freq,nlayers)
        call write_nc(out_path, nSimuLen, outVars%tsl,"tsl","K", "soil temperature in each soil layer",str_freq,nlayers)
        call write_nc(out_path, nSimuLen, outVars%tsland,"tsland","K", "surface temperature",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%wtd,"wtd","m", "Water table depth",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%snd,"snd","m", "Total snow depth",str_freq,1)
        call write_nc(out_path, nSimuLen, outVars%lai,"lai","m2 m-2", "Leaf area index",str_freq,1)
        ! call write_nc(out_path, nSimuLen, all_gdd5_h,"GDD5","m2 m-2", "GDD5",str_freq,1)
        ! call write_nc(out_path, nSimuLen, all_onset_h,"onset","m2 m-2", "onset",str_freq,1)
        ! call write_nc(out_path, nSimuLen, all_storage_h,"storage","m2 m-2", "onset",str_freq,1)
        ! call write_nc(out_path, nSimuLen, all_add_h,"add","m2 m-2", "onset",str_freq,1)
        ! call write_nc(out_path, nSimuLen, all_accumulation_h,"accumulation","m2 m-2", "accumulation",str_freq,1)
        ! call write_nc(out_path, nSimuLen, all_test_h,"test_gpp","m2 m-2", "test_gpp",str_freq,9)
        
    end subroutine write_outputs_nc

    subroutine write_nc(outfile, nSimuLen, data, varName, unit, description, str_freq, nSoilLayer)
        IMPLICIT NONE
        real(kind=4), Dimension(nSimuLen, nSoilLayer), intent(in) :: data
        integer(kind=4) :: nSoilLayer
        integer(KIND=4) :: ncid, timid, dp_dimid, timvarid
        integer(kind=4) :: varid
        integer(kind=4), intent(in) :: nSimuLen
        CHARACTER(LEN=*), INTENT(IN) :: outfile, str_freq
        CHARACTER(len=*), intent(in) :: varName, unit, description
        character(len=:), allocatable :: nc_fileName
        character(len=100) :: timeUnit
        integer itime
        real, dimension(nSimuLen) :: time_values 
        integer :: start(1), count(1)
        
        allocate(character(len=200+len(outfile)) :: nc_fileName)
        nc_fileName = adjustl(trim(outfile))//"/"//adjustl(trim(varName))//"_"//str_freq//"_TECO-SPRUCE_"//&
            & adjustl(trim(case_name))//"_"//adjustl(trim(str_startyr))//"-"//adjustl(trim(str_endyr))//".nc"   
        
        !Create the netCDF file.
        CALL check(nf90_create(nc_fileName, NF90_CLOBBER, ncid))

        !Define the dimensions.
        ! CALL check(nf90_def_dim(ncid, "nSimu", nfreq,    simuid))
        CALL check(nf90_def_dim(ncid, "time",  nSimuLen, timid))
    
        if (nSoilLayer>1)then
            call check(nf90_def_dim(ncid, "depth", nSoilLayer, dp_dimid))
            CALL check(nf90_def_var(ncid = ncid, name = varName,  xtype = NF90_FLOAT, &
                & dimids = (/timid, dp_dimid/),  varID =varid))
        else
            CALL check(nf90_def_var(ncid = ncid, name = varName,  xtype = NF90_FLOAT, &
                & dimids = (/timid/),  varID =varid))
        endif

        call check(nf90_def_var(ncid, "time",  NF90_DOUBLE, timid,  timvarid))
        !Define data variable
        
        !Add attributes
        if (str_freq .eq. "hourly") then
            timeUnit = "hours since "//adjustl(trim(str_startyr))//"-01-01 00:00:00"
        else if (str_freq .eq. "daily") then
            timeUnit = "days since "//adjustl(trim(str_startyr))//"-01-01 00:00:00"
        else if (str_freq .eq. "monthly") then
            timeUnit = "months since "//adjustl(trim(str_startyr))//"-01-01 00:00:00"
        end if
        
        call check(nf90_put_att(ncid,timvarid,"units",adjustl(trim(timeUnit))))
        CALL check(nf90_put_att(ncid,varid,"units",unit))
        CALL check(nf90_put_att(ncid,varid,"description",description))
        CALL check(nf90_enddef(ncid)) 
        !End Definitions

        !Write Data
        ! if (nSoilLayer>1)then
        !     do i = 1, nSoilLayer
        !         CALL check(nf90_put_var(ncid, varid, data, start=[1,i], count=[nSimuLen,1]))
        !     enddo
        ! else

        do itime = 1, nSimuLen
            time_values(itime) = itime-1
        enddo
        start = 1
        count = nSimuLen

        CALL check(nf90_put_var(ncid, timvarid, time_values,start,count))
        CALL check(nf90_put_var(ncid, varid, data))
        
        CALL check(nf90_close(ncid))
    end subroutine write_nc

    ! check (ever so slightly modified from www.unidata.ucar.edu)
    subroutine check(istatus)
        ! use netcdf
        implicit none
        integer, intent(in) :: istatus
        if(istatus /= nf90_noerr) then
            write(*,*) trim(adjustl(nf90_strerror(istatus)))
        end if
    end subroutine check
#endif
end module io_mod