module MCMC_outputs
#ifdef USE_NETCDF
    use netcdf
#endif
    use datatypes
    use mcmc_mod
    implicit none
    ! This part of results will be stored in CSV-format

    type mcmc_outVars_type
        ! carbon fluxes (Kg C m-2 s-1)
        real, allocatable :: gpp(:, :)
        real, allocatable :: nee(:, :)
        real, allocatable :: npp(:, :)
        real, allocatable :: nppLeaf(:, :)
        real, allocatable :: nppWood(:, :)
        real, allocatable :: nppStem(:, :)
        real, allocatable :: nppRoot(:, :)
        real, allocatable :: nppOther(:, :)           ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        real, allocatable :: ra(:, :)
        real, allocatable :: raLeaf(:, :)
        real, allocatable :: raStem(:, :)
        real, allocatable :: raRoot(:, :)
        real, allocatable :: raOther(:, :)
        real, allocatable :: rMaint(:, :)
        real, allocatable :: rGrowth(:, :)            ! maintenance respiration and growth respiration
        real, allocatable :: rh(:, :)
        real, allocatable :: nbp(:, :)                ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        real, allocatable :: wetlandCH4(:, :)
        real, allocatable :: wetlandCH4prod(:, :)
        real, allocatable :: wetlandCH4cons(:, :)     ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        real, allocatable :: cLeaf(:, :)
        real, allocatable :: cStem(:, :)
        real, allocatable :: cRoot(:, :)
        real, allocatable :: cOther(:, :)              ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        real, allocatable :: cLitter(:, :)
        real, allocatable :: cLitterCwd(:, :)          ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        real, allocatable :: cSoil(:, :)
        real, allocatable :: cSoilLevels(:, :, :)
        real, allocatable :: cSoilFast(:, :)
        real, allocatable :: cSoilSlow(:, :)
        real, allocatable :: cSoilPassive(:, :)           ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        real, allocatable :: CH4(:, :, :)          ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        real, allocatable :: fBNF(:, :)
        real, allocatable :: fN2O(:, :)
        real, allocatable :: fNloss(:, :)
        real, allocatable :: fNnetmin(:, :)
        real, allocatable :: fNdep(:, :)                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        real, allocatable :: nLeaf(:, :)
        real, allocatable :: nStem(:, :)
        real, allocatable :: nRoot(:, :)
        real, allocatable :: nOther(:, :)
        real, allocatable :: nLitter(:, :)
        real, allocatable :: nLitterCwd(:, :)
        real, allocatable :: nSoil(:, :)
        real, allocatable :: nMineral(:, :)                ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        real, allocatable :: hfls(:, :)
        real, allocatable :: hfss(:, :)
        real, allocatable :: SWnet(:, :)
        real, allocatable :: LWnet(:, :)                   ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        real, allocatable :: ec(:, :)
        real, allocatable :: tran(:, :)
        real, allocatable :: es(:, :)                      ! Canopy evaporation; Canopy transpiration; Soil evaporation
        real, allocatable :: hfsbl(:, :)                   ! Snow sublimation
        real, allocatable :: mrro(:, :)
        real, allocatable :: mrros(:, :)
        real, allocatable :: mrrob(:, :)                   ! Total runoff; Surface runoff; Subsurface runoff
        ! other
        real, allocatable :: mrso(:, :, :)           ! Kg m-2, soil moisture in each soil layer
        real, allocatable :: tsl(:, :, :)            ! K, soil temperature in each soil layer
        real, allocatable :: tsland(:, :)                  ! K, surface temperature
        real, allocatable :: wtd(:, :)                     ! m, Water table depth
        real, allocatable :: snd(:, :)                     ! m, Total snow depth
        real, allocatable :: lai(:, :)                     ! m2 m-2, Leaf area index            
    end type mcmc_outVars_type

    type params_sets
        real, allocatable :: tot_paramsets(:,:), upg_paramsets(:,:), sel_paramsets(:,:) 
    end type params_sets
    type(params_sets), allocatable :: arr_params_set(:)
    
    type(mcmc_outVars_type) sel_paramsets_outs_h
    type(mcmc_outVars_type) sel_paramsets_outs_d
    type(mcmc_outVars_type) sel_paramsets_outs_m
    ! total simulation outputs
    type(mcmc_outVars_type) tot_paramsets_outs_h
    type(mcmc_outVars_type) tot_paramsets_outs_d
    type(mcmc_outVars_type) tot_paramsets_outs_m
    
    CHARACTER(len=4) :: str_startyr, str_endyr
    ! integer, parameter :: nRand = 20
    ! integer, dimension(nRand) :: rand_number
    integer, allocatable :: rand_number(:)

contains

    subroutine init_mcmc_outputs(nDAsimu, npar4DA)
        implicit none
        integer, intent(in) :: nDAsimu, npar4DA
        integer :: ipft, npft

        allocate(rand_number(nRand))

        write(str_startyr,"(I4)")forcing(1)%year
        write(str_endyr,"(I4)")forcing(nforcing)%year

        allocate(arr_params_set(npft))
        do ipft = 1, npft
            allocate(arr_params_set(ipft)%tot_paramsets(nDAsimu,npar4DA))
            allocate(arr_params_set(ipft)%sel_paramsets(nRand, npar4DA))    ! select 500 parameter sets
        enddo

        if (do_mc_out_hr) then
            call allocate_mcmc_outs_type(nRand, nHours,  sel_paramsets_outs_h)
            call allocate_mcmc_outs_type(nDAsimu, nHours,  tot_paramsets_outs_h)
        endif
        if (do_mc_out_day) then
            call allocate_mcmc_outs_type(nRand, nDays,   sel_paramsets_outs_d)
            call allocate_mcmc_outs_type(nDAsimu, nDays,   tot_paramsets_outs_d)
        endif
        if (do_mc_out_mon) then
            call allocate_mcmc_outs_type(nRand, nMonths, sel_paramsets_outs_m)
            call allocate_mcmc_outs_type(nDAsimu, nMonths, tot_paramsets_outs_m)
        endif
        ! allocate the total simulation results
    end subroutine init_mcmc_outputs

    subroutine mcmc_param_outputs(nUpgraded, npar4DA, parnames, DAparidx)
        implicit none
        integer, intent(in) :: nUpgraded, npar4DA
        integer nBuilt_in, ipar, nline, iline, inum
        character(250) :: outfile_mc_ParamSets
        character(*), intent(in) :: parnames(:)
        integer, intent(in) :: DAparidx(:)
        character(20), allocatable :: DA_parname(:)
        character(1200) :: header_line
        integer :: ipft, npft

        allocate(DA_parname(npar4DA))
        header_line = ""
        do ipar = 1, npar4DA
            DA_parname(ipar) = parnames(DAparidx(ipar))
            header_line   = trim(header_line)//","//trim(parnames(DAparidx(ipar)))
        enddo

        ! delete the built-in
        nBuilt_in = int(0.1*nUpgraded)
        if (nBuilt_in .lt. 1) nBuilt_in = 1
        do ipft = 1, npft
            allocate(arr_params_set(ipft)%upg_paramsets(nUpgraded - nBuilt_in, npar4DA))
            arr_params_set(ipft)%upg_paramsets = arr_params_set(ipft)%tot_paramsets(nBuilt_in:nUpgraded, :)
            ! write(*,*)"test_all", nBuilt_in, nUpgraded, size(tot_paramsets,1), size(tot_paramsets,2)
            outfile_mc_ParamSets = adjustl(trim(outDir_mcmc))//"/"//adjustl(trim("total_parameter_sets_"))&
                //adjustl(trim(spec_names(ipft)))//adjustl(trim(".txt"))
            open(118, file=outfile_mc_ParamSets, status='replace')
            write(118, *) header_line(2:)
            do iline = 1, size(arr_params_set(ipft)%upg_paramsets, 1)
                write(118, '(*(ES10.3,:,","))') arr_params_set(ipft)%upg_paramsets(iline,:)
            enddo
            close(118)
        enddo

        ! choose the random 100 parameter sets and simulations
        call generate_random_numbers(1, nUpgraded - nBuilt_in, rand_number)

        do ipft = 1, npft
            do inum = 1, nRand
                arr_params_set(ipft)%sel_paramsets(inum, :) = arr_params_set(ipft)%upg_paramsets(rand_number(inum),:)
                if (do_mc_out_hr) then
                    call select_mcmc_simu_outputs(rand_number(inum), inum, tot_paramsets_outs_h, sel_paramsets_outs_h)
                endif
                if (do_mc_out_day) then
                    call select_mcmc_simu_outputs(rand_number(inum), inum, tot_paramsets_outs_d, sel_paramsets_outs_d)
                endif
                if (do_mc_out_mon) then
                    call select_mcmc_simu_outputs(rand_number(inum), inum, tot_paramsets_outs_m, sel_paramsets_outs_m)
                endif
            enddo

            outfile_mc_ParamSets = adjustl(trim(outDir_mcmc))//"/"//adjustl(trim("sel_parameter_sets_"))&
                //adjustl(trim(spec_names(ipft)))//adjustl(trim(".txt"))
            open(137, file=outfile_mc_ParamSets, status='replace')
            write(137, *) header_line(2:)
            do iline = 1, nRand
                write(137, '(*(ES10.3,:,","))') arr_params_set(ipft)%sel_paramsets(iline,:)
            enddo
            close(137)
        enddo

        ! save the selected simulations to nc format results
        if (do_mc_out_hr) then
            call write_outputs_nc(outDir_mcmc_h, nRand, nHours,  sel_paramsets_outs_h, "hourly")
        endif
        if (do_mc_out_day) then
            call write_outputs_nc(outDir_mcmc_d, nRand, nDays,   sel_paramsets_outs_d, "daily")
        endif
        if (do_mc_out_mon) then
            call write_outputs_nc(outDir_mcmc_m, nRand, nMonths, sel_paramsets_outs_m, "monthly")
        endif

        ! deallocate
        deallocate(DA_parname)
        do ipft = 1, npft
            deallocate(arr_params_set(ipft)%upg_paramsets)
        enddo
    end subroutine mcmc_param_outputs

    subroutine select_mcmc_simu_outputs(idx_tot, idx_sel, total_simus, selected_simus)
        implicit none
        integer, intent(in) :: idx_tot, idx_sel
        type(mcmc_outVars_type), intent(in) :: total_simus
        type(mcmc_outVars_type), intent(inout) :: selected_simus
        
        selected_simus%gpp(idx_sel, :)            = total_simus%gpp(idx_tot, :)   
        selected_simus%nee(idx_sel, :)            = total_simus%nee(idx_tot, :)
        selected_simus%npp(idx_sel, :)            = total_simus%npp(idx_tot, :)
        selected_simus%nppLeaf(idx_sel, :)        = total_simus%nppLeaf(idx_tot, :)
        selected_simus%nppWood(idx_sel, :)        = total_simus%nppWood(idx_tot, :)
        selected_simus%nppStem(idx_sel, :)        = total_simus%nppStem(idx_tot, :)
        selected_simus%nppRoot(idx_sel, :)        = total_simus%nppRoot(idx_tot, :)
        selected_simus%nppOther(idx_sel, :)       = total_simus%nppOther(idx_tot, :)
        selected_simus%ra(idx_sel, :)             = total_simus%ra(idx_tot, :)
        selected_simus%raLeaf(idx_sel, :)         = total_simus%raLeaf(idx_tot, :)
        selected_simus%raStem(idx_sel, :)         = total_simus%raStem(idx_tot, :)
        selected_simus%raRoot(idx_sel, :)         = total_simus%raRoot(idx_tot, :)
        selected_simus%raOther(idx_sel, :)        = total_simus%raOther(idx_tot, :)
        selected_simus%rMaint(idx_sel, :)         = total_simus%rMaint(idx_tot, :)
        selected_simus%rGrowth(idx_sel, :)        = total_simus%rGrowth(idx_tot, :)
        selected_simus%rh(idx_sel, :)             = total_simus%rh(idx_tot, :)
        selected_simus%nbp(idx_sel, :)            = total_simus%nbp(idx_tot, :)
        selected_simus%wetlandCH4(idx_sel, :)     = total_simus%wetlandCH4(idx_tot, :)
        selected_simus%wetlandCH4prod(idx_sel, :) = total_simus%wetlandCH4prod(idx_tot, :)
        selected_simus%wetlandCH4cons(idx_sel, :) = total_simus%wetlandCH4cons(idx_tot, :)
        ! Carbon Pools  (KgC m-2)
        selected_simus%cLeaf(idx_sel, :)          = total_simus%cLeaf(idx_tot, :)
        selected_simus%cStem(idx_sel, :)          = total_simus%cStem(idx_tot, :)
        selected_simus%cRoot(idx_sel, :)          = total_simus%cRoot(idx_tot, :)
        selected_simus%cOther(idx_sel, :)         = total_simus%cOther(idx_tot, :)
        selected_simus%cLitter(idx_sel, :)        = total_simus%cLitter(idx_tot, :)
        selected_simus%cLitterCwd(idx_sel, :)     = total_simus%cLitterCwd(idx_tot, :)
        selected_simus%cSoil(idx_sel, :)          = total_simus%cSoil(idx_tot, :)
        selected_simus%cSoilLevels(idx_sel, :, :) = total_simus%cSoilLevels(idx_tot,:, :)
        selected_simus%cSoilFast(idx_sel, :)      = total_simus%cSoilFast(idx_tot, :)
        selected_simus%cSoilSlow(idx_sel, :)      = total_simus%cSoilSlow(idx_tot, :)
        selected_simus%cSoilPassive(idx_sel, :)   = total_simus%cSoilPassive(idx_tot, :)
        selected_simus%CH4(idx_sel, :, :)         = total_simus%CH4(idx_tot, :, :)
        ! Nitrogen fluxes (kgN m-2 s-1)
        selected_simus%fBNF(idx_sel, :)           = total_simus%fBNF(idx_tot, :)
        selected_simus%fN2O(idx_sel, :)           = total_simus%fN2O(idx_tot, :)
        selected_simus%fNloss(idx_sel, :)         = total_simus%fNloss(idx_tot, :)
        selected_simus%fNnetmin(idx_sel, :)       = total_simus%fNnetmin(idx_tot, :)
        selected_simus%fNdep(idx_sel, :)          = total_simus% fNdep(idx_tot, :)
        ! Nitrogen pools (kgN m-2)
        selected_simus%nLeaf(idx_sel, :)          = total_simus%nLeaf(idx_tot, :)
        selected_simus%nStem(idx_sel, :)          = total_simus%nStem(idx_tot, :)
        selected_simus%nRoot(idx_sel, :)          = total_simus%nRoot(idx_tot, :)
        selected_simus%nOther(idx_sel, :)         = total_simus%nOther(idx_tot, :)
        selected_simus%nLitter(idx_sel, :)        = total_simus%nLitter(idx_tot, :)
        selected_simus%nLitterCwd(idx_sel, :)     = total_simus%nLitterCwd(idx_tot, :)
        selected_simus%nSoil(idx_sel, :)          = total_simus%nSoil(idx_tot, :)
        selected_simus%nMineral(idx_sel, :)       = total_simus%nMineral(idx_tot, :)
        ! energy fluxes (W m-2)
        selected_simus%hfls(idx_sel, :)           = total_simus%hfls(idx_tot, :)
        selected_simus%hfss(idx_sel, :)           = total_simus%hfss(idx_tot, :)
        selected_simus%SWnet(idx_sel, :)          = total_simus%SWnet(idx_tot, :)
        selected_simus%LWnet(idx_sel, :)          = total_simus%LWnet(idx_tot, :)
        ! water fluxes (kg m-2 s-1)
        selected_simus%ec(idx_sel, :)             = total_simus%ec(idx_tot, :)
        selected_simus%tran(idx_sel, :)           = total_simus%tran(idx_tot, :)
        selected_simus%es(idx_sel, :)             = total_simus%es(idx_tot, :)
        selected_simus%hfsbl(idx_sel, :)          = total_simus%hfsbl(idx_tot, :)
        selected_simus%mrro(idx_sel, :)           = total_simus%mrro(idx_tot, :)
        selected_simus%mrros(idx_sel, :)          = total_simus%mrros(idx_tot, :)
        selected_simus%mrrob(idx_sel, :)          = total_simus%mrrob(idx_tot, :)
        ! other
        selected_simus%mrso(idx_sel, :, :)        = total_simus%mrso(idx_tot,:, :)
        selected_simus%tsl(idx_sel, :, :)         = total_simus%tsl(idx_tot,:, :)
        selected_simus%tsland(idx_sel, :)         = total_simus%tsland(idx_tot, :)
        selected_simus%wtd(idx_sel, :)            = total_simus%wtd(idx_tot, :)
        selected_simus%snd(idx_sel, :)            = total_simus%snd(idx_tot, :)
        selected_simus%lai(idx_sel, :)            = total_simus%lai(idx_tot, :)
        return
    end subroutine select_mcmc_simu_outputs

    ! subroutine mcmc_update_outputs(upgraded, dataType, simu_outputs)
    !     implicit none
    !     integer, intent(in) :: upgraded
    !     type(mcmc_outVars_type), intent(inout)   :: dataType
    !     type(outvars_data_type), intent(in) :: simu_outputs

    !     dataType%gpp(upgraded, :)            = simu_outputs%gpp    
    !     dataType%nee(upgraded, :)            = simu_outputs%nee
    !     dataType%npp(upgraded, :)            = simu_outputs%npp
    !     dataType%nppLeaf(upgraded, :)        = simu_outputs%nppLeaf
    !     dataType%nppWood(upgraded, :)        = simu_outputs%nppWood
    !     dataType%nppStem(upgraded, :)        = simu_outputs%nppStem
    !     dataType%nppRoot(upgraded, :)        = simu_outputs%nppRoot
    !     dataType%nppOther(upgraded, :)       = simu_outputs%nppOther
    !     dataType%ra(upgraded, :)             = simu_outputs%ra
    !     dataType%raLeaf(upgraded, :)         = simu_outputs%raLeaf
    !     dataType%raStem(upgraded, :)         = simu_outputs%raStem
    !     dataType%raRoot(upgraded, :)         = simu_outputs%raRoot
    !     dataType%raOther(upgraded, :)        = simu_outputs%raOther
    !     dataType%rMaint(upgraded, :)         = simu_outputs%rMaint
    !     dataType%rGrowth(upgraded, :)        = simu_outputs%rGrowth
    !     dataType%rh(upgraded, :)             = simu_outputs%rh
    !     dataType%nbp(upgraded, :)            = simu_outputs%nbp
    !     dataType%wetlandCH4(upgraded, :)     = simu_outputs%wetlandCH4
    !     dataType%wetlandCH4prod(upgraded, :) = simu_outputs%wetlandCH4prod
    !     dataType%wetlandCH4cons(upgraded, :) = simu_outputs%wetlandCH4cons
    !     ! Carbon Pools  (KgC m-2)
    !     dataType%cLeaf(upgraded, :)          = simu_outputs%cLeaf
    !     dataType%cStem(upgraded, :)          = simu_outputs%cStem
    !     dataType%cRoot(upgraded, :)          = simu_outputs%cRoot
    !     dataType%cOther(upgraded, :)         = simu_outputs%cOther
    !     dataType%cLitter(upgraded, :)        = simu_outputs%cLitter
    !     dataType%cLitterCwd(upgraded, :)     = simu_outputs%cLitterCwd
    !     dataType%cSoil(upgraded, :)          = simu_outputs%cSoil
    !     dataType%cSoilLevels(upgraded, :, :) = simu_outputs%cSoilLevels
    !     dataType%cSoilFast(upgraded, :)      = simu_outputs%cSoilFast
    !     dataType%cSoilSlow(upgraded, :)      = simu_outputs%cSoilSlow
    !     dataType%cSoilPassive(upgraded, :)   = simu_outputs%cSoilPassive
    !     dataType%CH4(upgraded, :, :)         = simu_outputs%CH4
    !     ! Nitrogen fluxes (kgN m-2 s-1)
    !     dataType%fBNF(upgraded, :)           = simu_outputs%fBNF
    !     dataType%fN2O(upgraded, :)           = simu_outputs%fN2O
    !     dataType%fNloss(upgraded, :)         = simu_outputs%fNloss
    !     dataType%fNnetmin(upgraded, :)       = simu_outputs%fNnetmin
    !     dataType%fNdep(upgraded, :)          = simu_outputs% fNdep
    !     ! Nitrogen pools (kgN m-2)
    !     dataType%nLeaf(upgraded, :)          = simu_outputs%nLeaf
    !     dataType%nStem(upgraded, :)          = simu_outputs%nStem
    !     dataType%nRoot(upgraded, :)          = simu_outputs%nRoot
    !     dataType%nOther(upgraded, :)         = simu_outputs%nOther
    !     dataType%nLitter(upgraded, :)        = simu_outputs%nLitter
    !     dataType%nLitterCwd(upgraded, :)     = simu_outputs%nLitterCwd
    !     dataType%nSoil(upgraded, :)          = simu_outputs%nSoil
    !     dataType%nMineral(upgraded, :)       = simu_outputs%nMineral
    !     ! energy fluxes (W m-2)
    !     dataType%hfls(upgraded, :)           = simu_outputs%hfls
    !     dataType%hfss(upgraded, :)           = simu_outputs%hfss
    !     dataType%SWnet(upgraded, :)          = simu_outputs%SWnet
    !     dataType%LWnet(upgraded, :)          = simu_outputs%LWnet
    !     ! water fluxes (kg m-2 s-1)
    !     dataType%ec(upgraded, :)             = simu_outputs%ec
    !     dataType%tran(upgraded, :)           = simu_outputs%tran
    !     dataType%es(upgraded, :)             = simu_outputs%es
    !     dataType%hfsbl(upgraded, :)          = simu_outputs%hfsbl
    !     dataType%mrro(upgraded, :)           = simu_outputs%mrro
    !     dataType%mrros(upgraded, :)          = simu_outputs%mrros
    !     dataType%mrrob(upgraded, :)          = simu_outputs%mrrob
    !     ! other
    !     dataType%mrso(upgraded, :, :)        = simu_outputs%mrso
    !     dataType%tsl(upgraded, :, :)         = simu_outputs%tsl
    !     dataType%tsland(upgraded, :)         = simu_outputs%tsland
    !     dataType%wtd(upgraded, :)            = simu_outputs%wtd
    !     dataType%snd(upgraded, :)            = simu_outputs%snd
    !     dataType%lai(upgraded, :)            = simu_outputs%lai
    !     return
    ! end subroutine mcmc_update_outputs

    subroutine write_outputs_nc(mc_outdir, ntime, nSimuLen, write_data, str_freq)
        implicit none
        character(*), intent(in) :: mc_outdir, str_freq
        integer, intent(in) :: ntime, nSimuLen
        type(mcmc_outVars_type), intent(in) :: write_data

        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%gpp,    &
            "gpp",     "kgC m-2 s-1", "gross primary productivity", str_freq, 1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%npp,    &
            "npp",     "kgC m-2 s-1", "Total net primary productivity",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%nppLeaf, &
            "nppLeaf", "kgC m-2 s-1", "NPP allocated to leaf tissues",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%nppWood, &
            "nppWood", "kgC m-2 s-1", "NPP allocated to above ground woody tissues",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%nppStem, &
            "nppStem","kgC m-2 s-1", "NPP allocated to stem tissues",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%nppRoot, &
            "nppRoot","kgC m-2 s-1", "NPP allocated to root tissues",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%nppOther, &
            "nppOther","kgC m-2 s-1", "NPP allocated to other plant organs (reserves, fruits, exudates)",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%ra,       &
            "ra","kgC m-2 s-1", "Plant Autotrophic Respiration",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%raLeaf,   &
            "raLeaf","kgC m-2 s-1", "Ra from leaves",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%raStem,   &
            "raStem","kgC m-2 s-1", "Ra from above ground woody tissues",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%raRoot,   &
            "raRoot","kgC m-2 s-1", "Ra from fine roots",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%raOther,  &
            "raOther","kgC m-2 s-1", "Ra from other plant organs (reserves, fruits, exudates)",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%rMaint,         &
            "rMaint","kgC m-2 s-1", "Maintenance respiration",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%rGrowth,        &
            "rGrowth","kgC m-2 s-1", "Growth respiration",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%rh,             &
            "rh","kgC m-2 s-1", "Heterotrophic respiration rate",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%nbp,            &
            "nbp","kgC m-2 s-1", "Net Biome productivity (NBP = GPP - Rh - Ra - other losses)",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%wetlandCH4,     &
            "wetlandCH4","kgC m-2 s-1", "Net fluxes of CH4",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%wetlandCH4prod, &
            "wetlandCH4prod","kgC m-2 s-1", "CH4 production",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%wetlandCH4cons, &
            "wetlandCH4cons","kgC m-2 s-1", "CH4 consumption",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%cLeaf,          &
            "cLeaf","kgC m-2", "Carbon biomass in leaves",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%cStem,          &
            "cStem","kgC m-2", "Carbon above ground woody biomass",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%cRoot,          &
            "cRoot","kgC m-2", "Carbon biomass in roots",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%cOther,         &
            "cOther","kgC m-2", "Carbon biomass in other plant organs (reserves, fruits)",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%cLitter,        &
            "cLitter","kgC m-2", "Carbon in litter (excluding coarse woody debris)",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%cLitterCwd,     &
            "cLitterCwd","kgC m-2", "Carbon in coarse woody debris",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%cSoil,          &
            "cSoil","kgC m-2", "Total soil organic carbon",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%cSoilLevels,    &
            "cSoilLevels","kgC m-2", "Depth-specific soil organic carbon",str_freq,nlayers)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%cSoilFast,      &
            "cSoilFast","kgC m-2", "Fast soil organic carbon",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%cSoilSlow,      &
            "cSoilSlow","kgC m-2 s-1", "Slow soil organic carbon",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%cSoilPassive,   &
            "cSoilPassive","kgC m-2 s-1", "Passive soil organic carbon",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%CH4,            &
            "CH4","kgC m-2 s-1", "Methane concentration",str_freq,nlayers)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%fBNF,           &
            "fBNF","kgN m-2 s-1", "biological nitrogen fixation",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%fN2O,           &
            "fN2O","kgN m-2 s-1", "loss of nitrogen through emission of N2O",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%fNloss,         &
            "fNloss","kgN m-2 s-1", "Total loss of nitrogen to the atmosphere and from leaching",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%fNnetmin,       &
            "fNnetmin","kgN m-2 s-1", "net mineralization of N",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%fNdep,          &
            "fNdep","kgN m-2 s-1", "Nitrogen deposition",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%nLeaf,          &
            "nLeaf","kgN m-2", "Nitrogen in leaves",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%nStem,          &
            "nStem","kgN m-2", "Nitrogen in stems",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%nRoot,          &
            "nRoot","kgN m-2", "Nirogen in roots",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%nOther,         &
            "nOther","kgN m-2", "nitrogen in other plant organs (reserves, fruits)",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%nLitter,        &
            "nLitter","kgN m-2", "Nitrogen in litter (excluding coarse woody debris)",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%nLitterCwd,     &
            "nLitterCwd","kgN m-2", "Nitrogen in coarse woody debris",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%nSoil,     &
            "nSoil","kgN m-2", "Nitrogen in soil organic matter",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%nMineral,  &
            "nMineral","kgN m-2", "Mineral nitrogen pool",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%hfls,    &
            "hfls","W m-2", "Sensible heat flux",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%hfss,    &
            "hfss","W m-2", "Latent heat flux",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%SWnet,   &
            "SWnet","W m-2", "Net shortwave radiation",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%LWnet,   &
            "LWnet","W m-2", "Net longwave radiation",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%ec,      &
            "ec","kg m-2 s-1", "Canopy evaporation",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%tran,    &
            "tran","kg m-2 s-1", "Canopy transpiration",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%es,      &
            "es","kg m-2 s-1", "Soil evaporation",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%hfsbl,   &
            "hfsbl","kg m-2 s-1", "Snow sublimation",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%mrro,    &
            "mrro","kg m-2 s-1", "Total runoff",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%mrros,   &
            "mrros","kg m-2 s-1", "Surface runoff",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%mrrob,   &
            "mrrob","kg m-2 s-1", "Subsurface runoff",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%mrso,    &
            "mrso","kg m-2", "soil moisture in each soil layer",str_freq,nlayers)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%tsl,     &
            "tsl","K", "soil temperature in each soil layer",str_freq,nlayers)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%tsland,  &
            "tsland","K", "surface temperature",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%wtd,     &
            "wtd","m", "Water table depth",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%snd,     &
            "snd","m", "Total snow depth",str_freq,1)
        call write_mcmc_nc(mc_outdir, ntime, nSimuLen, write_data%lai,     &
            "lai","m2 m-2", "Leaf area index",str_freq,1)

    end subroutine write_outputs_nc

    subroutine write_mcmc_nc(outfile, ntime, nSimuLen, data, varName, unit, description, freq, nSoilLayer)
        IMPLICIT NONE
        real(kind=4), Dimension(ntime, nSimuLen, nSoilLayer), intent(in) :: data
        integer(kind=4) :: nSoilLayer
        integer(KIND=4) :: ncid, simuid, timid, dp_dimid, simuvarid, timvarid
        integer(kind=4) :: varid
        integer(kind=4), intent(in) :: ntime, nSimuLen
        CHARACTER(LEN=*), INTENT(IN) :: outfile, freq
        CHARACTER(len=*), intent(in) :: varName, unit, description
        character(len=:), allocatable :: nc_fileName
        character(len=100) :: timeUnit
        integer itime
        real, dimension(nSimuLen) :: time_values 
        integer nsimu_values(nRand)
        integer :: start(1), count(1)
        
        allocate(character(len=200+len(outfile)) :: nc_fileName)
        nc_fileName = adjustl(trim(outfile))//"/"//adjustl(trim(varName))//"_"//freq//"_TECO-SPRUCE_"//&
            & adjustl(trim(case_name))//"_"//adjustl(trim(str_startyr))//"-"//adjustl(trim(str_endyr))//".nc"   
        
        !Create the netCDF file.
        CALL check_mc(nf90_create(nc_fileName, NF90_CLOBBER, ncid))

        !Define the dimensions.
        CALL check_mc(nf90_def_dim(ncid, "nSimu", ntime,    simuid))
        CALL check_mc(nf90_def_dim(ncid, "time",  nSimuLen, timid))
    
        if (nSoilLayer>1)then
            call check_mc(nf90_def_dim(ncid, "depth", nSoilLayer, dp_dimid))
            CALL check_mc(nf90_def_var(ncid = ncid, name = varName,  xtype = NF90_FLOAT, &
                & dimids = (/simuid, timid, dp_dimid/),  varID =varid))
        else
            CALL check_mc(nf90_def_var(ncid = ncid, name = varName,  xtype = NF90_FLOAT, &
                & dimids = (/simuid, timid/),  varID =varid))
        endif
        call check_mc(nf90_def_var(ncid, "nSimu", NF90_DOUBLE, simuid, simuvarid))
        call check_mc(nf90_def_var(ncid, "time",  NF90_DOUBLE, timid,  timvarid))
        !Define data variable
        
        !Add attributes
        if (freq .eq. "hourly") then
            timeUnit = "hours since "//adjustl(trim(str_startyr))//"-01-01 00:00:00"
        else if (freq .eq. "daily") then
            timeUnit = "days since "//adjustl(trim(str_startyr))//"-01-01 00:00:00"
        else if (freq .eq. "monthly") then
            timeUnit = "months since "//adjustl(trim(str_startyr))//"-01-01 00:00:00"
        end if
        
        ! call check_mc(nf90_put_att(ncid,simuvarid,"",adjustl(trim(timeUnit))))
        call check_mc(nf90_put_att(ncid,timvarid,"units",adjustl(trim(timeUnit))))
        CALL check_mc(nf90_put_att(ncid,varid,"units",unit))
        CALL check_mc(nf90_put_att(ncid,varid,"description",description))
        CALL check_mc(nf90_enddef(ncid)) 
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
        do itime = 1, nRand
            nsimu_values(itime) = itime
        enddo
        call check_mc(nf90_put_var(ncid, simuvarid, nsimu_values))
        CALL check_mc(nf90_put_var(ncid, timvarid, time_values,start,count))
        CALL check_mc(nf90_put_var(ncid, varid, data))
        
        CALL check_mc(nf90_close(ncid))
    end subroutine write_mcmc_nc

    ! check (ever so slightly modified from www.unidata.ucar.edu)
    subroutine check_mc(istatus)
        ! use netcdf
        implicit none
        integer, intent(in) :: istatus
        if(istatus /= nf90_noerr) then
            write(*,*) trim(adjustl(nf90_strerror(istatus)))
        end if
    end subroutine check_mc

    subroutine generate_random_numbers(min_value, max_value, res_rand)
        implicit none
        integer, dimension(:), intent(inout) :: res_rand
        integer, intent(in) :: min_value, max_value
        integer :: i, j, temp, range_size, available_numbers
        integer, dimension(max_value - min_value + 1) :: all_numbers
        real :: r

        ! initialize the random
        call random_seed()

        ! initilize all_numbers array
        do i = 1, size(all_numbers)
            all_numbers(i) = min_value - 1 + i
        end do

        ! using Fisher-Yates method
        do i = size(all_numbers), 2, -1
            call random_number(r)
            j = int(r * i) + 1
            temp = all_numbers(i)
            all_numbers(i) = all_numbers(j)
            all_numbers(j) = temp
        end do

        ! get the before N random number 
        res_rand = all_numbers(1:size(res_rand,1))
    end subroutine generate_random_numbers

    subroutine allocate_mcmc_outs_type(ntime, nSimuLen, dataType)
        implicit none
        ! 
        integer, intent(in) :: ntime, nSimuLen
        type(mcmc_outVars_type), intent(out) :: dataType
        allocate(dataType%gpp(ntime, nSimuLen))
        allocate(dataType%nee(ntime, nSimuLen))
        allocate(dataType%npp(ntime, nSimuLen))
        allocate(dataType%nppLeaf(ntime, nSimuLen))
        allocate(dataType%nppWood(ntime, nSimuLen))
        allocate(dataType%nppStem(ntime, nSimuLen))
        allocate(dataType%nppRoot(ntime, nSimuLen))
        allocate(dataType%nppOther(ntime, nSimuLen))           ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        allocate(dataType%ra(ntime, nSimuLen))
        allocate(dataType%raLeaf(ntime, nSimuLen))
        allocate(dataType%raStem(ntime, nSimuLen))
        allocate(dataType%raRoot(ntime, nSimuLen))
        allocate(dataType%raOther(ntime, nSimuLen))
        allocate(dataType%rMaint(ntime, nSimuLen))
        allocate(dataType%rGrowth(ntime, nSimuLen))            ! maintenance respiration and growth respiration
        allocate(dataType%rh(ntime, nSimuLen))
        allocate(dataType%nbp(ntime, nSimuLen))                ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        allocate(dataType%wetlandCH4(ntime, nSimuLen))
        allocate(dataType%wetlandCH4prod(ntime, nSimuLen))
        allocate(dataType%wetlandCH4cons(ntime, nSimuLen))     ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        allocate(dataType%cLeaf(ntime, nSimuLen))
        allocate(dataType%cStem(ntime, nSimuLen))
        allocate(dataType%cRoot(ntime, nSimuLen))
        allocate(dataType%cOther(ntime, nSimuLen))              ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        allocate(dataType%cLitter(ntime, nSimuLen))
        allocate(dataType%cLitterCwd(ntime, nSimuLen))          ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        allocate(dataType%cSoil(ntime, nSimuLen))
        allocate(dataType%cSoilLevels(ntime, nSimuLen, nlayers))
        allocate(dataType%cSoilFast(ntime, nSimuLen))
        allocate(dataType%cSoilSlow(ntime, nSimuLen))
        allocate(dataType%cSoilPassive(ntime, nSimuLen))           ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        allocate(dataType%CH4(ntime, nSimuLen, nlayers))          ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        allocate(dataType%fBNF(ntime, nSimuLen))
        allocate(dataType%fN2O(ntime, nSimuLen))
        allocate(dataType%fNloss(ntime, nSimuLen))
        allocate(dataType%fNnetmin(ntime, nSimuLen))
        allocate(dataType%fNdep(ntime, nSimuLen))                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        allocate(dataType%nLeaf(ntime, nSimuLen))
        allocate(dataType%nStem(ntime, nSimuLen))
        allocate(dataType%nRoot(ntime, nSimuLen))
        allocate(dataType%nOther(ntime, nSimuLen))
        allocate(dataType%nLitter(ntime, nSimuLen))
        allocate(dataType%nLitterCwd(ntime, nSimuLen))
        allocate(dataType%nSoil(ntime, nSimuLen))
        allocate(dataType%nMineral(ntime, nSimuLen))                ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        allocate(dataType%hfls(ntime, nSimuLen))
        allocate(dataType%hfss(ntime, nSimuLen))
        allocate(dataType%SWnet(ntime, nSimuLen))
        allocate(dataType%LWnet(ntime, nSimuLen))                   ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        allocate(dataType%ec(ntime, nSimuLen))
        allocate(dataType%tran(ntime, nSimuLen))
        allocate(dataType%es(ntime, nSimuLen))                      ! Canopy evaporation; Canopy transpiration; Soil evaporation
        allocate(dataType%hfsbl(ntime, nSimuLen))                   ! Snow sublimation
        allocate(dataType%mrro(ntime, nSimuLen))
        allocate(dataType%mrros(ntime, nSimuLen))
        allocate(dataType%mrrob(ntime, nSimuLen))                   ! Total runoff; Surface runoff; Subsurface runoff
        ! other
        allocate(dataType%mrso(ntime, nSimuLen, nlayers))           ! Kg m-2, soil moisture in each soil layer
        allocate(dataType%tsl(ntime, nSimuLen, nlayers))            ! K, soil temperature in each soil layer
        allocate(dataType%tsland(ntime, nSimuLen))                  ! K, surface temperature
        allocate(dataType%wtd(ntime, nSimuLen))                     ! m, Water table depth
        allocate(dataType%snd(ntime, nSimuLen))                     ! m, Total snow depth
        allocate(dataType%lai(ntime, nSimuLen))
    end subroutine allocate_mcmc_outs_type

    subroutine deallocate_mcmc_outs_type(dataType)
        type(mcmc_outVars_type), intent(inout) :: dataType

        if(allocated(rand_number)) deallocate(rand_number)

        if (allocated(dataType%gpp))            deallocate(dataType%gpp)
        if (allocated(dataType%nee))            deallocate(dataType%nee)
        if (allocated(dataType%npp))            deallocate(dataType%npp)
        if (allocated(dataType%nppLeaf))        deallocate(dataType%nppLeaf)
        if (allocated(dataType%nppWood))        deallocate(dataType%nppWood)
        if (allocated(dataType%nppStem))        deallocate(dataType%nppStem)
        if (allocated(dataType%nppRoot))        deallocate(dataType%nppRoot)
        if (allocated(dataType%nppOther))       deallocate(dataType%nppOther)           
        if (allocated(dataType%ra))             deallocate(dataType%ra)
        if (allocated(dataType%raLeaf))         deallocate(dataType%raLeaf)
        if (allocated(dataType%raStem))         deallocate(dataType%raStem)
        if (allocated(dataType%raRoot))         deallocate(dataType%raRoot)
        if (allocated(dataType%raOther))        deallocate(dataType%raOther)
        if (allocated(dataType%rMaint))         deallocate(dataType%rMaint)
        if (allocated(dataType%rGrowth))        deallocate(dataType%rGrowth)           
        if (allocated(dataType%rh))             deallocate(dataType%rh)
        if (allocated(dataType%nbp))            deallocate(dataType%nbp)                
        if (allocated(dataType%wetlandCH4))     deallocate(dataType%wetlandCH4)
        if (allocated(dataType%wetlandCH4prod)) deallocate(dataType%wetlandCH4prod)
        if (allocated(dataType%wetlandCH4cons)) deallocate(dataType%wetlandCH4cons)  
        ! Carbon Pools  (KgC m-2)
        if (allocated(dataType%cLeaf))        deallocate(dataType%cLeaf)
        if (allocated(dataType%cStem))        deallocate(dataType%cStem)
        if (allocated(dataType%cRoot))        deallocate(dataType%cRoot)
        if (allocated(dataType%cOther))       deallocate(dataType%cOther)
        if (allocated(dataType%cLitter))      deallocate(dataType%cLitter)
        if (allocated(dataType%cLitterCwd))   deallocate(dataType%cLitterCwd)
        if (allocated(dataType%cSoil))        deallocate(dataType%cSoil)
        if (allocated(dataType%cSoilLevels))  deallocate(dataType%cSoilLevels)
        if (allocated(dataType%cSoilFast))    deallocate(dataType%cSoilFast)
        if (allocated(dataType%cSoilSlow))    deallocate(dataType%cSoilSlow)
        if (allocated(dataType%cSoilPassive)) deallocate(dataType%cSoilPassive)
        if (allocated(dataType%CH4))          deallocate(dataType%CH4)
        ! Nitrogen fluxes (kgN m-2 s-1)
        if (allocated(dataType%fBNF))         deallocate(dataType%fBNF)
        if (allocated(dataType%fN2O))         deallocate(dataType%fN2O)
        if (allocated(dataType%fNloss))       deallocate(dataType%fNloss)
        if (allocated(dataType%fNnetmin))     deallocate(dataType%fNnetmin)
        if (allocated(dataType%fNdep))        deallocate(dataType%fNdep)
        ! Nitrogen pools (kgN m-2)
        if (allocated(dataType%nLeaf))        deallocate(dataType%nLeaf)
        if (allocated(dataType%nStem))        deallocate(dataType%nStem)
        if (allocated(dataType%nRoot))        deallocate(dataType%nRoot)
        if (allocated(dataType%nOther))       deallocate(dataType%nOther)
        if (allocated(dataType%nLitter))      deallocate(dataType%nLitter)
        if (allocated(dataType%nLitterCwd))   deallocate(dataType%nLitterCwd)
        if (allocated(dataType%nSoil))        deallocate(dataType%nSoil)
        if (allocated(dataType%nMineral))     deallocate(dataType%nMineral)
        ! energy fluxes (W m-2)
        if (allocated(dataType%hfls))         deallocate(dataType%hfls)
        if (allocated(dataType%hfss))         deallocate(dataType%hfss)
        if (allocated(dataType%SWnet))        deallocate(dataType%SWnet)
        if (allocated(dataType%LWnet))        deallocate(dataType%LWnet)
        ! water fluxes (kg m-2 s-1)
        if (allocated(dataType%ec))           deallocate(dataType%ec)
        if (allocated(dataType%tran))         deallocate(dataType%tran)
        if (allocated(dataType%es))           deallocate(dataType%es)
        if (allocated(dataType%hfsbl))        deallocate(dataType%hfsbl)
        if (allocated(dataType%mrro))         deallocate(dataType%mrro)
        if (allocated(dataType%mrros))        deallocate(dataType%mrros)
        if (allocated(dataType%mrrob))        deallocate(dataType%mrrob)
        ! other
        if (allocated(dataType%mrso))         deallocate(dataType%mrso)      
        if (allocated(dataType%tsl))          deallocate(dataType%tsl)       
        if (allocated(dataType%tsland))       deallocate(dataType%tsland) 
        if (allocated(dataType%wtd))          deallocate(dataType%wtd)
        if (allocated(dataType%snd))          deallocate(dataType%snd)
        if (allocated(dataType%lai))          deallocate(dataType%lai)
    end subroutine deallocate_mcmc_outs_type

end module MCMC_outputs