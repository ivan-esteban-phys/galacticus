!! Copyright 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
!!    Andrew Benson <abenson@obs.carnegiescience.edu>
!!
!! This file is part of Galacticus.
!!
!!    Galacticus is free software: you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.
!!
!!    Galacticus is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.
!!
!!    You should have received a copy of the GNU General Public License
!!    along with Galacticus.  If not, see <http://www.gnu.org/licenses/>.

!% Contains a module which implements calculations of projected correlation functions using the halo model.

module Halo_Model_Projected_Correlations
  !% Implements calculations of projected correlation functions using the halo model.
  private
  public :: Halo_Model_Projected_Correlation

  abstract interface
     double precision function integrandWeight(x)
       double precision, intent(in   ) :: x
     end function integrandWeight
  end interface

contains

  subroutine Halo_Model_Projected_Correlation(                                              &
       &                                      conditionalMassFunction_                    , &
       &                                      projectedSeparationBinned                   , &
       &                                      projectedCorrelationFunctionMassMinimum     , &
       &                                      projectedCorrelationFunctionMassMaximum     , &
       &                                      projectedCorrelationFunctionHaloMassMinimum , &
       &                                      projectedCorrelationFunctionHaloMassMaximum , &
       &                                      projectedCorrelationFunctionLineOfSightDepth, &
       &                                      projectedCorrelationFunctionHalfIntegral    , &
       &                                      projectedCorrelationBinned                    &
       &                                     )
    !% Compute the projected correlation function of galaxies above a specified mass using the halo model.
    use, intrinsic :: ISO_C_Binding
    use               FGSL
    use               Memory_Management
    use               Galacticus_Error
    use               Geometry_Surveys
    use               Galacticus_Nodes
    use               Cosmology_Functions
    use               Conditional_Mass_Functions
    use               Numerical_Integration
    use               Dark_Matter_Profiles_Concentration
    use               Node_Component_Dark_Matter_Profile_Scale
    use               Dark_Matter_Halo_Scales
    use               Numerical_Ranges
    use               Numerical_Constants_Math
    use               Power_Spectra
    use               FFTLogs
    use               Tables
    use               Table_Labels
    use               Linear_Growth
    use               Halo_Mass_Functions
    implicit none
    class           (conditionalMassFunctionClass       ), intent(inout)                                             :: conditionalMassFunction_
    double precision                                     , intent(in   ), dimension(                             : ) :: projectedSeparationBinned
    double precision                                     , intent(in   )                                             :: projectedCorrelationFunctionMassMinimum     , projectedCorrelationFunctionMassMaximum    , &
       &                                                                                                                projectedCorrelationFunctionHaloMassMinimum , projectedCorrelationFunctionHaloMassMaximum, &
       &                                                                                                                projectedCorrelationFunctionLineOfSightDepth
    logical                                              , intent(in   )                                             :: projectedCorrelationFunctionHalfIntegral
    double precision                                     , intent(  out), dimension(size(projectedSeparationBinned)) :: projectedCorrelationBinned
    class           (powerSpectrumClass                 ), pointer                                                   :: powerSpectrum_
    class           (cosmologyFunctionsClass            ), pointer                                                   :: cosmologyFunctions_
    class           (surveyGeometryClass                ), pointer                                                   :: surveyGeometry_
    class           (darkMatterProfileConcentrationClass), pointer                                                   :: darkMatterProfileConcentration_
    type            (treeNode                           ), pointer                                                   :: thisNode
    class           (nodeComponentBasic                 ), pointer                                                   :: thisBasic
    class           (nodeComponentDarkMatterProfile     ), pointer                                                   :: thisDarkMatterProfile
    class           (darkMatterHaloScaleClass           ), pointer                                                   :: darkMatterHaloScale_
    class           (linearGrowthClass                  ), pointer                                                   :: linearGrowth_
    class           (haloMassFunctionClass              ), pointer                                                   :: haloMassFunction_
    procedure       (integrandWeight                    ), pointer                                                   :: integrandWeightFunction
    double precision                                     , allocatable  , dimension(                             : ) :: powerSpectrumOneHalo                              , powerSpectrumTwoHalo                              , &
         &                                                                                                              wavenumber                                        , powerSpectrumTotal                                , &
         &                                                                                                              separation                                        , correlation                                       , &
         &                                                                                                              projectedCorrelation
    integer                                              , parameter                                                 :: wavenumberCountPerDecade                   =10
    double precision                                     , parameter                                                 :: wavenumberMinimum                          =1.0d-3, wavenumberMaximum                          =1.0d4, &
         &                                                                                                              virialWavenumberMultiplierMaximum          =1.0d+3
    logical                                                                                                          :: integrationWarningIssued=.false.
    double precision                                                                                                 :: time                                              , timeMinimum                                       , &
         &                                                                                                              expansionFactor                                   , volume                                            , &
         &                                                                                                              timeMaximum                                       , galaxyDensity                                     , &
         &                                                                                                              projectedSeparation                               , binSeparationMaximum                              , &
         &                                                                                                              binWidthLogarithmic                               , binSeparationMinimum
    integer                                                                                                          :: iField                                            , iWavenumber                                       , &
         &                                                                                                              wavenumberCount                                   , iSeparation                                       , &
         &                                                                                                              projectedCorrelationFunctionSeparationCount
    type            (fgsl_function                      )                                                            :: integrandFunction
    type            (fgsl_integration_workspace         )                                                            :: integrationWorkspace
    type            (c_ptr                              )                                                            :: parameterPointer
    logical                                                                                                          :: integrationReset
    type            (table1DLogarithmicLinear           )                                                            :: correlationTable

    ! Get required objects.
    cosmologyFunctions_             => cosmologyFunctions            ()     
    surveyGeometry_                 => surveyGeometry                ()
    darkMatterProfileConcentration_ => darkMatterProfileConcentration()
    darkMatterHaloScale_            => darkMatterHaloScale           ()
    linearGrowth_                   => linearGrowth                  ()
    haloMassFunction_               => haloMassFunction              ()
    powerSpectrum_                  => powerSpectrum                 ()
    ! Create worker node.
    thisNode              => treeNode                  (                 )
    thisBasic             => thisNode%basic            (autoCreate=.true.)
    thisDarkMatterProfile => thisNode%darkMatterProfile(autoCreate=.true.)
    select type (thisDarkMatterProfile)
    type is (nodeComponentDarkMatterProfileScale)
       ! This is acceptable.
    class default
          ! This is not.
       call Galacticus_Error_Report('Projected_Correlation_Function','this code expects to use the "scale" dark matter profile component')
    end select
    ! Generate wavenumber range.
    wavenumberCount=int(log10(wavenumberMaximum/wavenumberMinimum)*dble(wavenumberCountPerDecade))+1
    wavenumber     =Make_Range(wavenumberMinimum,wavenumberMaximum,wavenumberCount,rangeTypeLogarithmic)
    call Alloc_Array(powerSpectrumTotal  ,[wavenumberCount])
    call Alloc_Array(powerSpectrumOneHalo,[wavenumberCount])
    call Alloc_Array(powerSpectrumTwoHalo,[wavenumberCount])
    ! Initialize.
    volume              =0.0d0
    galaxyDensity       =0.0d0
    powerSpectrumOneHalo=0.0d0
    powerSpectrumTwoHalo=0.0d0
    ! Iterate over survey fields.
    do iField=1,surveyGeometry_%fieldCount()
       ! Find time range for volume-limited sample.
       timeMinimum=cosmologyFunctions_%timeAtDistanceComoving(surveyGeometry_%distanceMaximum(projectedCorrelationFunctionMassMinimum,iField))
       timeMaximum=cosmologyFunctions_%timeAtDistanceComoving(surveyGeometry_%distanceMinimum(projectedCorrelationFunctionMassMinimum,iField))
       ! Integrate the volume term.
       integrationReset    =.true.
       volume              =                                       &
            & +volume                                              &
            & +Integrate(                                          &
            &            timeMinimum                             , &
            &            timeMaximum                             , &
            &            volumeTimeIntegrand                     , &
            &            parameterPointer                        , &
            &            integrandFunction                       , &
            &            integrationWorkspace                    , &
            &            toleranceRelative                =1.0d-2, &
            &            reset=integrationReset                    &
            &           )                                          &
            & *surveyGeometry_%solidAngle(iField)
       call Integrate_Done(integrandFunction,integrationWorkspace)
       ! Integrate the normalization term.
       integrationReset    =.true.
       galaxyDensity       =                                       &
            & +galaxyDensity                                       &
            & +Integrate(                                          &
            &            timeMinimum                             , &
            &            timeMaximum                             , &
            &            normalizationTimeIntegrand              , &
            &            parameterPointer                        , &
            &            integrandFunction                       , &
            &            integrationWorkspace                    , &
            &            toleranceRelative                =1.0d-2, &
            &            reset=integrationReset                    &
            &           )                                          &
            & *surveyGeometry_%solidAngle(iField)
       call Integrate_Done(integrandFunction,integrationWorkspace)
       ! Iterate over wavenumbers.
       do iWavenumber=1,wavenumberCount 
          ! Integrate the one-halo term.
          integrationReset    =.true.
          powerSpectrumOneHalo        (iWavenumber)=                              &
               & +powerSpectrumOneHalo(iWavenumber)                               &
               & +Integrate(                                                      &
               &            timeMinimum                                         , &
               &            timeMaximum                                         , &
               &            powerSpectrumOneHaloTimeIntegrand                   , &
               &            parameterPointer                                    , &
               &            integrandFunction                                   , &
               &            integrationWorkspace                                , &
               &            toleranceRelative                =1.0d-2            , &
               &            reset                            =integrationReset  , &
               &            integrationRule                  =FGSL_Integ_Gauss61  &
               &           )                                                      &
               & *surveyGeometry_%solidAngle(iField)
          call Integrate_Done(integrandFunction,integrationWorkspace)
          ! Integrate the two-halo term.
          integrationReset    =.true.
          powerSpectrumTwoHalo        (iWavenumber)=                              &
               & +powerSpectrumTwoHalo(iWavenumber)                               &
               & +Integrate(                                                      &
               &            timeMinimum                                         , &
               &            timeMaximum                                         , &
               &            powerSpectrumTwoHaloTimeIntegrand                   , &
               &            parameterPointer                                    , &
               &            integrandFunction                                   , &
               &            integrationWorkspace                                , &
               &            toleranceRelative                =1.0d-2            , &
               &            reset                            =integrationReset  , &
               &            integrationRule                  =FGSL_Integ_Gauss61  &
               &           )                                                      &
               & *surveyGeometry_%solidAngle(iField)
          call Integrate_Done(integrandFunction,integrationWorkspace)
       end do
    end do
    ! Check for non-zero galaxy density.
    if (galaxyDensity > 0.0d0) then
       ! Construct the net power spectrum.
       galaxyDensity       =galaxyDensity       /volume
       powerSpectrumOneHalo=powerSpectrumOneHalo/volume
       powerSpectrumTwoHalo=powerSpectrumTwoHalo/volume
       do iWavenumber=1,wavenumberCount
          powerSpectrumTotal(iWavenumber)=(                                                  &
               &                           +powerSpectrumOneHalo(           iWavenumber )    &
               &                           +powerSpectrum_%power(wavenumber(iWavenumber))    &
               &                           *powerSpectrumTwoHalo(           iWavenumber )**2 &
               &                          )                                                  &
               &                          /galaxyDensity                                 **2
       end do
       ! Fourier transform to get the correlation function.
       call Alloc_Array(correlation,shape(wavenumber))
       call Alloc_Array(separation ,shape(wavenumber))
       call FFTLog(                     &
            &      wavenumber         , &
            &      separation         , &
            &      +powerSpectrumTotal  &
            &      *wavenumber          &
            &      * 4.0d0*Pi           &
            &      /(2.0d0*Pi)**3     , &
            &      correlation        , &
            &      fftLogSine         , &
            &      fftLogForward        &
            &     )
       correlation=correlation/separation
       ! Project the correlation function.
       call Alloc_Array(projectedCorrelation,shape(wavenumber))
       call correlationTable%create(separation(1),separation(wavenumberCount),wavenumberCount,extrapolationTypeExtrapolate)
       integrandWeightFunction => projectionIntegrandWeight
       do iSeparation=1,wavenumberCount
          projectedSeparation=separation(iSeparation)
          projectedCorrelation          (iSeparation)=sum(                                                                                            &
               &                                           correlationTable%integrationWeights(                                                       &
               &                                                                               projectedSeparation                                  , &
               &                                                                               sqrt(                                                  &
               &                                                                                    +projectedSeparation                         **2  &
               &                                                                                    +projectedCorrelationFunctionLineOfSightDepth**2  &
               &                                                                                   )                                                , &
               &                                                                               integrandWeightFunction                                &
               &                                                                              )                                                       &
               &                                          *correlation                                                                                &
               &                                         )
       end do
       ! If the integral was taken over the half range, 0<pi<pi_max, rather than the full range, -pi_max<pi<pi_max, then divide
       ! the projected correlation function by two.
       if (projectedCorrelationFunctionHalfIntegral) projectedCorrelation=projectedCorrelation/2.0d0
       ! Average the projected correlation function into bins.
       projectedCorrelationFunctionSeparationCount=size(projectedSeparationBinned)
       binWidthLogarithmic=log(projectedSeparationBinned(2)/projectedSeparationBinned(1))
       integrandWeightFunction => binningIntegrandWeight
       do iSeparation=1,projectedCorrelationFunctionSeparationCount
          binSeparationMinimum         =projectedSeparationBinned(iSeparation)*exp(-0.5d0*binWidthLogarithmic)
          binSeparationMaximum         =projectedSeparationBinned(iSeparation)*exp(+0.5d0*binWidthLogarithmic)
          projectedCorrelationBinned(iSeparation)=sum(                                                              &
               &                                       correlationTable%integrationWeights(                         &
               &                                                                           binSeparationMinimum   , &
               &                                                                           binSeparationMaximum   , &
               &                                                                           integrandWeightFunction  &
               &                                                                          )                         &
               &                                       /Pi                                                          &
               &                                       /(                                                           &
               &                                         +binSeparationMaximum**2                                   &
               &                                         -binSeparationMinimum**2                                   &
               &                                        )                                                           &
               &                                      *projectedCorrelation                                         &
               &                                     )
       end do
    else
       ! Galaxy density is zero. Return a zero correlation function.
       projectedCorrelationBinned=0.0d0
    end if

  contains  

    double precision function projectionIntegrandWeight(separation)
      !% The weight function applied to the correlation function when integrating to get the projected correlation function.
      implicit none
      double precision, intent(in   ) :: separation

      if (separation > projectedSeparation) then
         projectionIntegrandWeight=2.0d0*separation/sqrt(separation**2-projectedSeparation**2)
      else
         projectionIntegrandWeight=0.0d0
      end if
      return
    end function projectionIntegrandWeight

    double precision function binningIntegrandWeight(separation)
      !% The weight function applied to the projected correlation function when integrating into bins.
      implicit none
      double precision, intent(in   ) :: separation

      binningIntegrandWeight=2.0d0*Pi*separation
      return
    end function binningIntegrandWeight

    function powerSpectrumOneHaloTimeIntegrand(timePrime,parameterPointer) bind(c)
      !% Time integrand for the one-halo term in the power spectrum.
      use Galacticus_Display
      implicit none
      real   (c_double                  )        :: powerSpectrumOneHaloTimeIntegrand
      real   (c_double                  ), value :: timePrime
      type   (c_ptr                     ), value :: parameterPointer
      type   (fgsl_function             )        :: integrandFunctionTime
      type   (fgsl_integration_workspace)        :: integrationWorkspaceTime
      logical                                    :: integrationResetTime
      integer                                    :: errorStatus

      time           =timePrime
      expansionFactor=cosmologyFunctions_%expansionFactor(time)
      call thisBasic%timeSet                             (time)
      call thisBasic%timeLastIsolatedSet                 (time)
      integrationResetTime             =.true.
      powerSpectrumOneHaloTimeIntegrand=                                                  &
           & +Integrate(                                                                  &
           &            projectedCorrelationFunctionHaloMassMinimum                     , &
           &            projectedCorrelationFunctionHaloMassMaximum                     , &
           &            powerSpectrumOneHaloIntegrand                                   , &
           &            parameterPointer                                                , &
           &            integrandFunctionTime                                           , &
           &            integrationWorkspaceTime                                        , &
           &            toleranceRelative                          =1.0d-2              , &
           &            reset                                      =integrationResetTime, &
           &            integrationRule                            =FGSL_Integ_Gauss61  , &
           &            errorStatus                                =errorStatus           &
           &           )                                                                  &
           & *cosmologyFunctions_%comovingVolumeElementTime(time)
      call Integrate_Done(integrandFunctionTime,integrationWorkspaceTime)
      if (errorStatus /= errorStatusSuccess .and. .not.integrationWarningIssued) then
         call Galacticus_Display_Message('WARNING: [powerSpectrumOneHaloTimeIntegrand] integration failed - likely due to oscillatory nature of integrand - proceeding anyway',verbosity=verbosityWarn)
         integrationWarningIssued=.true.
      end if
      return
    end function powerSpectrumOneHaloTimeIntegrand

    function powerSpectrumOneHaloIntegrand(massHalo,parameterPointer) bind(c)
      !% Integrand for the one-halo term in the power spectrum.
      use Dark_Matter_Halo_Biases
      use Dark_Matter_Profiles
      use Dark_Matter_Profile_Scales
      use Galacticus_Calculations_Resets
      implicit none
      real            (c_double              )          :: powerSpectrumOneHaloIntegrand
      real            (c_double              ), value   :: massHalo
      type            (c_ptr                 ), value   :: parameterPointer
      class           (darkMatterProfileClass), pointer :: darkMatterProfile_
      class           (haloMassFunctionClass ), pointer :: haloMassFunction_
      double precision                                  :: darkMatterProfileKSpace      , numberCentrals, numberSatellites, wavenumberMaximum

      darkMatterProfile_ => darkMatterProfile()
      call Galacticus_Calculations_Reset(thisNode)
      call thisBasic            % massSet(massHalo                           )
      call Galacticus_Calculations_Reset(thisNode)
      call thisDarkMatterProfile%scaleSet(Dark_Matter_Profile_Scale(thisNode))
      ! Return zero if we're more than some maximum factor above the virial wavenumber for this halo. This avoids attempting to
      ! integrate rapidly oscillating Fourier profiles.
      wavenumberMaximum=virialWavenumberMultiplierMaximum/(darkMatterHaloScale_%virialRadius(thisNode)/expansionFactor)
      if (waveNumber(iWavenumber) > wavenumberMaximum) then
         powerSpectrumOneHaloIntegrand=0.0d0
      else
         darkMatterProfileKSpace=darkMatterProfile_%kSpace(thisNode,waveNumber(iWavenumber)/expansionFactor)
         numberCentrals         =max(                                                                                                                       &
              &                      +0.0d0                                                                                                               , &
              &                      +conditionalMassFunction_%massFunction(massHalo,projectedCorrelationFunctionMassMinimum,haloModelGalaxyTypeCentral  )  &
              &                      -conditionalMassFunction_%massFunction(massHalo,projectedCorrelationFunctionMassMaximum,haloModelGalaxyTypeCentral  )  &
              &                     )
         numberSatellites       =max(                                                                                                                       &
              &                      +0.0d0                                                                                                               , &
              &                      +conditionalMassFunction_%massFunction(massHalo,projectedCorrelationFunctionMassMinimum,haloModelGalaxyTypeSatellite)  &
              &                      -conditionalMassFunction_%massFunction(massHalo,projectedCorrelationFunctionMassMaximum,haloModelGalaxyTypeSatellite)  &
              &                     )
         ! Note that we include 2 times the central-satellite term since we want to count each pair twice (i.e. central-satellite and
         ! then satellite-central). This is consistent with the N(N-1) counting for the satellite-satellite term, and with the
         ! counting in the two-halo term.
         powerSpectrumOneHaloIntegrand=                        &
              & +haloMassFunction_%differential(time,massHalo) &
              & *(                                             &
              &   +darkMatterProfileKSpace                     &
              &   *2.0d0                                       & 
              &   *numberCentrals                              &
              &   *numberSatellites                            &
              &   +darkMatterProfileKSpace**2                  &
              &   *numberSatellites       **2                  &
              &  )
      end if
      return
    end function powerSpectrumOneHaloIntegrand

    function powerSpectrumTwoHaloTimeIntegrand(timePrime,parameterPointer) bind(c)
      !% Time integrand for the two-halo term in the power spectrum.
      use Galacticus_Display
      implicit none
      real   (c_double                  )          :: powerSpectrumTwoHaloTimeIntegrand
      real   (c_double                  ), value   :: timePrime
      type   (c_ptr                     ), value   :: parameterPointer
      type   (fgsl_function             )          :: integrandFunctionTime
      type   (fgsl_integration_workspace)          :: integrationWorkspaceTime
      logical                                      :: integrationResetTime
      integer                                      :: errorStatus

      time           =timePrime
      expansionFactor=cosmologyFunctions_%expansionFactor(time)
      call thisBasic%timeSet                             (time)
      call thisBasic%timeLastIsolatedSet                 (time)
      integrationResetTime             =.true.
      powerSpectrumTwoHaloTimeIntegrand=                                                  &
           & +Integrate(                                                                  &
           &            projectedCorrelationFunctionHaloMassMinimum                     , &
           &            projectedCorrelationFunctionHaloMassMaximum                     , &
           &            powerSpectrumTwoHaloIntegrand                                   , &
           &            parameterPointer                                                , &
           &            integrandFunctionTime                                           , &
           &            integrationWorkspaceTime                                        , &
           &            toleranceRelative                          =1.0d-2              , &
           &            reset                                      =integrationResetTime, &
           &            integrationRule                            =FGSL_Integ_Gauss61  , &
           &            errorStatus                                =errorStatus           &
           &           )                                                                  &
           & *linearGrowth_      %value                    (time)                         &
           & *cosmologyFunctions_%comovingVolumeElementTime(time)
      call Integrate_Done(integrandFunctionTime,integrationWorkspaceTime)
      if (errorStatus /= errorStatusSuccess .and. .not.integrationWarningIssued) then
         call Galacticus_Display_Message('WARNING: [powerSpectrumTwoHaloTimeIntegrand] integration failed - likely due to oscillatory nature of integrand - proceeding anyway',verbosity=verbosityWarn)
         integrationWarningIssued=.true.
      end if
      return
    end function powerSpectrumTwoHaloTimeIntegrand

    function powerSpectrumTwoHaloIntegrand(massHalo,parameterPointer) bind(c)
      !% Integrand for the two-halo term in the power spectrum.
      use Dark_Matter_Halo_Biases
      use Dark_Matter_Profiles
      use Galacticus_Calculations_Resets
      use Dark_Matter_Profile_Scales
      implicit none
      real            (c_double              )          :: powerSpectrumTwoHaloIntegrand
      real            (c_double              ), value   :: massHalo
      type            (c_ptr                 ), value   :: parameterPointer
      class           (darkMatterProfileClass), pointer :: darkMatterProfile_
      class           (haloMassFunctionClass ), pointer :: haloMassFunction_
      double precision                                  :: wavenumberMaximum

      darkMatterProfile_ => darkMatterProfile()
      haloMassFunction_  => haloMassFunction ()
      call Galacticus_Calculations_Reset(thisNode)
      call thisBasic            % massSet(massHalo                           )
      call Galacticus_Calculations_Reset(thisNode)
      call thisDarkMatterProfile%scaleSet(Dark_Matter_Profile_Scale(thisNode))
      ! Return zero if we're more than some maximum factor above the virial wavenumber for this halo. This avoids attempting to
      ! integrate rapidly oscillating Fourier profiles.
      wavenumberMaximum=virialWavenumberMultiplierMaximum/(darkMatterHaloScale_%virialRadius(thisNode)/expansionFactor)
      if (waveNumber(iWavenumber) > wavenumberMaximum) then
         powerSpectrumTwoHaloIntegrand=0.0d0
      else
         powerSpectrumTwoHaloIntegrand=                                                                        &
              & +haloMassFunction_%differential(time    ,massHalo                               )              &
              & *Dark_Matter_Halo_Bias         (thisNode                                        )              &
              & *darkMatterProfile_%kSpace     (thisNode,waveNumber(iWavenumber)/expansionFactor)              &
              & *max(                                                                                          &
              &      +0.0d0                                                                                  , &
              &      +conditionalMassFunction_%massFunction(massHalo,projectedCorrelationFunctionMassMinimum)  &
              &      -conditionalMassFunction_%massFunction(massHalo,projectedCorrelationFunctionMassMaximum)  &
              &     )
      end if
      return
    end function powerSpectrumTwoHaloIntegrand

    function normalizationTimeIntegrand(timePrime,parameterPointer) bind(c)
      !% Time integrand for the normalization term in the power spectrum.
      implicit none
      real   (c_double                  )        :: normalizationTimeIntegrand
      real   (c_double                  ), value :: timePrime
      type   (c_ptr                     ), value :: parameterPointer
      type   (fgsl_function             )        :: integrandFunctionTime
      type   (fgsl_integration_workspace)        :: integrationWorkspaceTime
      logical                                    :: integrationResetTime

      time           =timePrime
      integrationResetTime=.true.
      normalizationTimeIntegrand=                                           &
           & +Integrate(                                                    &
           &            projectedCorrelationFunctionHaloMassMinimum       , &
           &            projectedCorrelationFunctionHaloMassMaximum       , &
           &            normalizationIntegrand                            , &
           &            parameterPointer                                  , &
           &            integrandFunctionTime                             , &
           &            integrationWorkspaceTime                          , &
           &            toleranceRelative                          =1.0d-2, &
           &            reset=integrationResetTime                          &
           &           )                                                    &
           & *cosmologyFunctions_%comovingVolumeElementTime(time)
       call Integrate_Done(integrandFunctionTime,integrationWorkspaceTime)
      return
    end function normalizationTimeIntegrand

    function normalizationIntegrand(massHalo,parameterPointer) bind(c)
      !% Integrand for the normalization term in the power spectrum.
      implicit none
      real(c_double)        :: normalizationIntegrand
      real(c_double), value :: massHalo
      type(c_ptr   ), value :: parameterPointer

      normalizationIntegrand =                                                                              &
           & +haloMassFunction_%differential(time,massHalo)                                                 &
           & *max(                                                                                          &
           &      +0.0d0                                                                                  , &
           &      +conditionalMassFunction_%massFunction(massHalo,projectedCorrelationFunctionMassMinimum)  &
           &      -conditionalMassFunction_%massFunction(massHalo,projectedCorrelationFunctionMassMaximum)  &
           &     )
      return
    end function normalizationIntegrand

    function volumeTimeIntegrand(timePrime,parameterPointer) bind(c)
      !% Volume integrand for the normalization term in the power spectrum.
      implicit none
      real   (c_double                  )        :: volumeTimeIntegrand
      real   (c_double                  ), value :: timePrime
      type   (c_ptr                     ), value :: parameterPointer

      time               =timePrime
      volumeTimeIntegrand=cosmologyFunctions_%comovingVolumeElementTime(time)
      return
    end function volumeTimeIntegrand

  end subroutine Halo_Model_Projected_Correlation

end module Halo_Model_Projected_Correlations