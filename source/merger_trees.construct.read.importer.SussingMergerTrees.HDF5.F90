!! Copyright 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018,
!!           2019, 2020, 2021
!!    Andrew Benson <abenson@carnegiescience.edu>
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

  !% An implementation of the merger tree importer class for ``Sussing Merger Trees'' format merger tree files.

  use :: Cosmological_Density_Field, only : cosmologicalMassVarianceClass
  use :: IO_HDF5                   , only : hdf5Object

  !# <mergerTreeImporter name="mergerTreeImporterSussingHDF5">
  !#  <description>Importer for ``Sussing Merger Trees'' HDF5 format merger tree files (Thomas et al.; in prep.).</description>
  !# </mergerTreeImporter>
  type, extends(mergerTreeImporterSussing) :: mergerTreeImporterSussingHDF5
     !% A merger tree importer class for ``Sussing Merger Trees'' HDF5 format merger tree files (Thomas et al.; in prep.).
     private
     class(cosmologicalMassVarianceClass), pointer :: cosmologicalMassVariance_ => null()
     type (hdf5Object                   )          :: file                               , snapshots
   contains
     final     ::         sussingHDF5Destructor
     procedure :: open => sussingHDF5Open
     procedure :: load => sussingHDF5Load
  end type mergerTreeImporterSussingHDF5

  interface mergerTreeImporterSussingHDF5
     !% Constructors for the {\normalfont \ttfamily sussing} HDF5 format merger tree importer class.
     module procedure sussingHDF5ConstructorParameters
     module procedure sussingHDF5ConstructorInternal
  end interface mergerTreeImporterSussingHDF5

contains

  function sussingHDF5ConstructorParameters(parameters) result(self)
    !% Default constructor for the ``Sussing Merger Trees'' HDF5 format (Thomas et al.; in prep.) merger tree importer.
    use :: Input_Parameters, only : inputParameter, inputParameters
    implicit none
    type(mergerTreeImporterSussingHDF5)                :: self
    type(inputParameters              ), intent(inout) :: parameters

    !# <objectBuilder class="cosmologicalMassVariance" name="self%cosmologicalMassVariance_" source="parameters"/>
    self%mergerTreeImporterSussing=mergerTreeImporterSussing(parameters)
    !# <inputParametersValidate source="parameters"/>
    !# <objectDestructor name="self%cosmologicalMassVariance_"/>
    return
  end function sussingHDF5ConstructorParameters

  function sussingHDF5ConstructorInternal(fatalMismatches,fatalNonTreeNode,subvolumeCount,subvolumeBuffer,subvolumeIndex,badValue,badValueTest,treeSampleRate,massOption,cosmologyParameters_,cosmologyFunctions_,cosmologicalMassVariance_,randomNumberGenerator_) result(self)
    !% Default constructor for the ``Sussing Merger Trees'' HDF5 format (Thomas et al.; in prep.) merger tree importer.
    implicit none
    type            (mergerTreeImporterSussingHDF5)                              :: self
    class           (cosmologyParametersClass     ), intent(in   ), target       :: cosmologyParameters_
    class           (cosmologyFunctionsClass      ), intent(in   ), target       :: cosmologyFunctions_
    class           (cosmologicalMassVarianceClass), intent(in   ), target       :: cosmologicalMassVariance_
    class           (randomNumberGeneratorClass   ), intent(in   ), target       :: randomNumberGenerator_
    integer                                        , intent(in   ), dimension(3) :: subvolumeIndex
    logical                                        , intent(in   )               :: fatalMismatches          , fatalNonTreeNode
    integer                                        , intent(in   )               :: subvolumeCount           , badValueTest    , &
         &                                                                          massOption
    double precision                               , intent(in   )               :: subvolumeBuffer          , badValue        , &
         &                                                                          treeSampleRate
    !# <constructorAssign variables="*cosmologicalMassVariance_"/>

    self%mergerTreeImporterSussing=mergerTreeImporterSussing(fatalMismatches,fatalNonTreeNode,subvolumeCount,subvolumeBuffer,subvolumeIndex,badValue,badValueTest,treeSampleRate,massOption,cosmologyParameters_,cosmologyFunctions_,randomNumberGenerator_)
    return
  end function sussingHDF5ConstructorInternal

  subroutine sussingHDF5Destructor(self)
    !% Destructor for the {\normalfont \ttfamily sussing} HDF5 format merger tree importer class.
    use :: IO_HDF5, only : hdf5Access
    implicit none
    type(mergerTreeImporterSussingHDF5), intent(inout) :: self

    !$ call hdf5Access%set()
    if (self%snapshots%isOpen()) call self%snapshots%close()
    if (self%file     %isOpen()) call self%file     %close()
    !$ call hdf5Access%unset()
    !# <objectDestructor name="self%cosmologicalMassVariance_"/>
    return
  end subroutine sussingHDF5Destructor

  subroutine sussingHDF5Open(self,fileName)
    !% Validate a {\normalfont \ttfamily sussing} HDF5 format merger tree file.
    use :: Cosmology_Parameters            , only : hubbleUnitsLittleH
    use :: Display                         , only : displayMessage         , verbosityLevelWarn
    use :: Galacticus_Error                , only : Galacticus_Error_Report
    use :: IO_HDF5                         , only : hdf5Access
    use :: Memory_Management               , only : allocateArray          , deallocateArray
    use :: Numerical_Comparison            , only : Values_Differ
    use :: Numerical_Constants_Astronomical, only : megaParsec
    use :: String_Handling                 , only : operator(//)
    implicit none
    class           (mergerTreeImporterSussingHDF5), intent(inout)             :: self
    type            (varying_string               ), intent(in   )             :: fileName
    real                                           , allocatable, dimension(:) :: snapshotExpansionFactors
    type            (varying_string               )                            :: message
    character       (len=14                       )                            :: valueString
    integer                                                                    :: i
    double precision                                                           :: localLittleH0           , localOmegaMatter     , &
         &                                                                        localOmegaDE            , localSigma8          , &
         &                                                                        localOmegaBaryon        , fileOmegaBaryon      , &
         &                                                                        fileOmegaCDM            , fileLittleH0         , &
         &                                                                        fileOmegaDE             , fileSigma8

    ! Get cosmological parameters. We do this in advance to avoid HDF5 thread conflicts.
    localLittleH0   =self%cosmologyParameters_     %HubbleConstant (hubbleUnitsLittleH)
    localOmegaMatter=self%cosmologyParameters_     %OmegaMatter    (                  )
    localOmegaDE    =self%cosmologyParameters_     %OmegaDarkEnergy(                  )
    localOmegaBaryon=self%cosmologyParameters_     %OmegaBaryon    (                  )
    localSigma8     =self%cosmologicalMassVariance_%sigma8         (                  )
    !$ call hdf5Access%set()
    ! Open the HDF5 file.
    call self%file%openFile(char(fileName),overWrite=.false.)
    ! Open the snapshots group.
    self%snapshots=self%file%openGroup('Snapshots')
    ! Read expansion factors from the file.
    call self%snapshots%readTable('Snap','a',snapshotExpansionFactors)
    ! Convert expansion factors to times.
    call allocateArray(self%snapshotTimes,shape(snapshotExpansionFactors))
    do i=1,size(snapshotExpansionFactors)
       self%snapshotTimes(i)=self%cosmologyFunctions_%cosmicTime(dble(snapshotExpansionFactors(i)))
    end do
    call deallocateArray(snapshotExpansionFactors)
    ! Read cosmological parameters.
    call self%file%readAttribute('OmegaBaryon',fileOmegaBaryon,allowPseudoScalar=.true.)
    call self%file%readAttribute('OmegaCDM'   ,fileOmegaCDM   ,allowPseudoScalar=.true.)
    call self%file%readAttribute('OmegaLambda',fileOmegaDE    ,allowPseudoScalar=.true.)
    call self%file%readAttribute('H100'       ,fileLittleH0   ,allowPseudoScalar=.true.)
    call self%file%readAttribute('Sigma8'     ,fileSigma8     ,allowPseudoScalar=.true.)
    ! Read box size.
    call self%file%readAttribute('BoxsizeMpc' ,self%boxLength ,allowPseudoScalar=.true.)
    self%boxLengthUnits=importerUnits(.true.,megaParsec,-1,0)
    !$ call hdf5Access%unset()
    ! Verify cosmological parameters.
    if (Values_Differ(fileOmegaBaryon,localOmegaBaryon,absTol=0.0001d0)) then
       message='OmegaBaryon in merger tree file ['
       write (valueString,'(e14.8)') fileOmegaBaryon
       message=message//trim(valueString)//'] differs from the internal value ['
       write (valueString,'(e14.8)') localOmegaBaryon
       message=message//trim(valueString)//']'
       if (self%fatalMismatches) then
          call Galacticus_Error_Report(message//{introspection:location})
       else
          call displayMessage(message,verbosityLevelWarn)
       end if
    end if
    if (Values_Differ(fileOmegaCDM,localOmegaMatter-localOmegaBaryon,absTol=0.0001d0)) then
       message='OmegaCDM in merger tree file ['
       write (valueString,'(e14.8)') fileOmegaCDM
       message=message//trim(valueString)//'] differs from the internal value ['
       write (valueString,'(e14.8)') localOmegaMatter-localOmegaBaryon
       message=message//trim(valueString)//']'
       if (self%fatalMismatches) then
          call Galacticus_Error_Report(message//{introspection:location})
       else
          call displayMessage(message,verbosityLevelWarn)
       end if
    end if
    if (Values_Differ(fileOmegaDE,localOmegaDE,absTol=0.0001d0)) then
       message='OmegaLambda in merger tree file ['
       write (valueString,'(e14.8)') fileOmegaDE
       message=message//trim(valueString)//'] differs from the internal value ['
       write (valueString,'(e14.8)') localOmegaDE
       message=message//trim(valueString)//']'
       if (self%fatalMismatches) then
          call Galacticus_Error_Report(message//{introspection:location})
       else
          call displayMessage(message,verbosityLevelWarn)
       end if
    end if
    if (Values_Differ(fileLittleH0,localLittleH0,absTol=0.0001d0)) then
       message='H100 in merger tree file ['
       write (valueString,'(e14.8)') fileLittleH0
       message=message//trim(valueString)//'] differs from the internal value ['
       write (valueString,'(e14.8)') localLittleH0
       message=message//trim(valueString)//']'
       if (self%fatalMismatches) then
          call Galacticus_Error_Report(message//{introspection:location})
       else
          call displayMessage(message,verbosityLevelWarn)
       end if
    end if
    if (Values_Differ(fileSigma8,localSigma8,absTol=0.0001d0)) then
       message='Sigma8 in merger tree file ['
       write (valueString,'(e14.8)') fileSigma8
       message=message//trim(valueString)//'] differs from the internal value ['
       write (valueString,'(e14.8)') localSigma8
       message=message//trim(valueString)//']'
       if (self%fatalMismatches) then
          call Galacticus_Error_Report(message//{introspection:location})
       else
          call displayMessage(message,verbosityLevelWarn)
       end if
    end if
    return
  end subroutine sussingHDF5Open

  subroutine sussingHDF5Load(self,nodeSelfIndices,nodeIndexRanks,nodeDescendentLocations,nodeIncomplete,nodeCountTrees,nodeTreeIndices,treeIndicesAssigned,branchJumpCheckRequired,massUnits,lengthUnits,velocityUnits)
    !% Load a {\normalfont \ttfamily sussing} HDF5 format merger tree data.
    use            :: Arrays_Search    , only : searchIndexed
    use            :: Display          , only : displayCounter         , displayCounterClear, displayIndent, displayUnindent, &
          &                                     verbosityLevelWorking
    use            :: Galacticus_Error , only : Galacticus_Error_Report
    use, intrinsic :: ISO_C_Binding    , only : c_size_t
    use            :: Kind_Numbers     , only : kind_int8
    use            :: Memory_Management, only : allocateArray          , deallocateArray
    use            :: Sorting          , only : sortIndex
    use            :: String_Handling  , only : operator(//)
    implicit none
    class    (mergerTreeImporterSussingHDF5), intent(inout)                              :: self
    integer  (kind_int8                    ), intent(  out), dimension(:  ), allocatable :: nodeSelfIndices      , nodeTreeIndices
    integer  (c_size_t                     ), intent(  out), dimension(:  ), allocatable :: nodeIndexRanks       , nodeDescendentLocations
    logical                                 , intent(  out), dimension(:  ), allocatable :: nodeIncomplete
    integer  (kind=c_size_t                ), intent(  out)                              :: nodeCountTrees
    logical                                 , intent(  out)                              :: treeIndicesAssigned  , branchJumpCheckRequired
    type     (importerUnits                ), intent(  out)                              :: massUnits            , lengthUnits                , &
         &                                                                                  velocityUnits
    integer  (kind=kind_int8               )               , dimension(:  ), allocatable :: mergerTreeHaloIndices, mergerTreeDescendentIndices
    real                                                   , dimension(:  ), allocatable :: propertyReal
    integer                                                , dimension(:  ), allocatable :: propertyInteger
    integer  (kind=kind_int8               )               , dimension(:  ), allocatable :: propertyLongInteger
    character(len=32                       )               , dimension(:  ), allocatable :: propertyNames        , propertyUnitsText
    integer  (kind=c_size_t                )                                             :: i                    , j                          , &
         &                                                                                  nodeCount            , iProgenitor                , &
         &                                                                                  nodeCountSnapshot    , iHalo
    type     (varying_string               )                                             :: snapshotName
    type     (hdf5Object                   )                                             :: snapshot             , mergerTrees
    type     (importerUnits                )                                             :: propertyUnits
    logical                                                                              :: massUnitsAssigned    , lengthUnitsAssigned        , &
         &                                                                                  velocityUnitsAssigned
    integer                                                                              :: haloIndexOffset

    ! Display counter.
    call displayIndent ('Parsing "Sussing Merger Trees" HDF5 format merger tree file',verbosityLevelWorking)
    ! Count nodes in all snapshots.
    nodeCount=0
    do i=0,size(self%snapshotTimes)-1
       snapshotName='Snapshot'
       snapshotName=snapshotName//i
       snapshot    =self%snapshots%openGroup(char(snapshotName))
       call snapshot%readAttribute('NSnapHalo',nodeCountSnapshot,allowPseudoScalar=.true.)
       call snapshot%close()
       nodeCount=nodeCount+nodeCountSnapshot
    end do
    ! Abort if subvolumes are requested.
    if (self%subvolumeCount > 1) call Galacticus_Error_Report('this importer does not yet support subvolumes'//{introspection:location})
    ! Allocate nodes arrays.
    allocate(self%nodes(nodeCount))
    ! Read snapshots.
    call displayIndent('Reading snapshots',verbosityLevelWorking)
    nodeCount            =0
    massUnitsAssigned    =.false.
    lengthUnitsAssigned  =.false.
    velocityUnitsAssigned=.false.
    do i=0,size(self%snapshotTimes)-1
       call displayCounter(int(100.0d0*dble(i)/dble(size(self%snapshotTimes))),i==0,verbosityLevelWorking)
       snapshotName='Snapshot'
       snapshotName=snapshotName//i
       snapshot    =self%snapshots%openGroup(char(snapshotName))
       call snapshot%readAttribute('NSnapHalo',nodeCountSnapshot,allowPseudoScalar=.true.)
       if (nodeCountSnapshot > 0) then
          ! Read units.
          call snapshot%readTable('SnapHaloProp','name' ,propertyNames    )
          call snapshot%readTable('SnapHaloProp','units',propertyUnitsText)
          do j=1,size(propertyNames)
             select case (trim(propertyNames(j)))
             case ("Mvir")
                propertyUnits=decodeUnits(propertyUnitsText(j))
                if (propertyUnits%status) then
                   if (massUnitsAssigned    ) then
                      if (propertyUnits /= massUnits    ) call Galacticus_Error_Report('mismatch in mass units'//{introspection:location})
                   else
                      massUnits            =propertyUnits
                      massUnitsAssigned    =.true.
                   end if
                end if
             case ("Xc","Yc","Zc","Rvir","Rmax")
                propertyUnits=decodeUnits(propertyUnitsText(j))
                if (propertyUnits%status) then
                   if (lengthUnitsAssigned  ) then
                      if (propertyUnits /= lengthUnits  ) call Galacticus_Error_Report('mismatch in length units'//{introspection:location})
                   else
                      lengthUnits          =propertyUnits
                      lengthUnitsAssigned  =.true.
                   end if
                end if
             case ("Vxc","Vyc","Vzc","Vmax","v_esc","sigV")
                propertyUnits=decodeUnits(propertyUnitsText(j))
                if (propertyUnits%status) then
                   if (velocityUnitsAssigned) then
                      if (propertyUnits /= velocityUnits) call Galacticus_Error_Report('mismatch in velocity units'//{introspection:location})
                   else
                      velocityUnits        =propertyUnits
                      velocityUnitsAssigned=.true.
                   end if
                end if
             end select
          end do
          ! Read halo properties.
          call snapshot%readTable('SnapHalo','HaloID'  ,propertyLongInteger)
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%nodeIndex            =     propertyLongInteger
          call snapshot%readTable('SnapHalo','hostHalo',propertyLongInteger)
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%hostIndex            =     propertyLongInteger
          call snapshot%readTable('SnapHalo','npart'   ,propertyInteger    )
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%particleCount        =     propertyInteger
          call snapshot%readTable('SnapHalo','Mvir'    ,propertyReal       )
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%nodeMass             =dble(propertyReal       )
          call snapshot%readTable('SnapHalo','Vmax'    ,propertyReal       )
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%velocityMaximum      =dble(propertyReal       )
          call snapshot%readTable('SnapHalo','sigV'    ,propertyReal       )
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%velocityDispersion   =dble(propertyReal       )
          call snapshot%readTable('SnapHalo','Xc'      ,propertyReal       )
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%position          (1)=dble(propertyReal       )
          call snapshot%readTable('SnapHalo','Yc'      ,propertyReal       )
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%position          (2)=dble(propertyReal       )
          call snapshot%readTable('SnapHalo','Zc'      ,propertyReal       )
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%position          (3)=dble(propertyReal       )
          call snapshot%readTable('SnapHalo','Vxc'     ,propertyReal       )
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%velocity          (1)=dble(propertyReal       )
          call snapshot%readTable('SnapHalo','Vyc'     ,propertyReal       )
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%velocity          (2)=dble(propertyReal       )
          call snapshot%readTable('SnapHalo','Vzc'     ,propertyReal       )
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%velocity          (3)=dble(propertyReal       )
          call snapshot%readTable('SnapHalo','lambdaE' ,propertyReal       )
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%spin                 =dble(propertyReal       )
          call snapshot%readTable('SnapHalo','Lx'      ,propertyReal       )
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%spin3D            (1)=dble(propertyReal       )
          call snapshot%readTable('SnapHalo','Ly'      ,propertyReal       )
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%spin3D            (2)=dble(propertyReal       )
          call snapshot%readTable('SnapHalo','Lz'      ,propertyReal       )
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%spin3D            (3)=dble(propertyReal       )
          ! Compute scale radii.
          call snapshot%readTable('SnapHalo','Rvir'    ,propertyReal       )
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%scaleRadius          =dble(propertyReal       )
          call snapshot%readTable('SnapHalo','cNFW'    ,propertyReal       )
          do j=1,nodeCountSnapshot
             if (.not.self%valueIsBad(dble(propertyReal(j)))) then
                if (dble(propertyReal(j)) > 0.0d0) then
                   self%nodes(nodeCount+j)%scaleRadius=self%nodes(nodeCount+j)%scaleRadius/dble(propertyReal(j))
                else
                   self%nodes(nodeCount+j)%scaleRadius=0.0d0
                end if
             else
                self%scaleRadiiAvailableValue         =.false.
                self%nodes(nodeCount+j)%scaleRadius   =-1.0d0
             end if
          end do
          ! Set times.
          self%nodes(nodeCount+1:nodeCount+nodeCountSnapshot)%nodeTime             =self%snapshotTimes(i)
       end if
       call snapshot%close()
       nodeCount=nodeCount+nodeCountSnapshot
    end do
    call displayCounterClear(       verbosityLevelWorking)
    call displayUnindent     ('done',verbosityLevelWorking)
    ! Check that units were set.
    if (.not.    massUnitsAssigned) call Galacticus_Error_Report('mass units were not determined'    //{introspection:location})
    if (.not.  lengthUnitsAssigned) call Galacticus_Error_Report('length units were not determined'  //{introspection:location})
    if (.not.velocityUnitsAssigned) call Galacticus_Error_Report('velocity units were not determined'//{introspection:location})
    ! Check for bad values.
    do j=1,size(self%nodes)
       if     (                                          &
            &   self%valueIsBad(self%nodes(j)%spin     ) &
            &  .or.                                      &
            &   self%valueIsBad(self%nodes(j)%spin3D(1)) &
            &  .or.                                      &
            &   self%valueIsBad(self%nodes(j)%spin3D(2)) &
            &  .or.                                      &
            &   self%valueIsBad(self%nodes(j)%spin3D(3)) &
            & ) then
          self%spinsAvailableValue=.false.
          exit
       end if
    end do
    ! For halos with no host, assign them to be self-hosting.
    where (self%nodes%hostIndex <= 0)
       self%nodes%hostIndex=self%nodes%nodeIndex
    end where
    ! Scale 3D spins.
    forall(i=1:3)
       self%nodes%spin3D(i)=self%nodes%spin3D(i)*self%nodes%spin
    end forall
    ! Determine indices, ranks, and locations.
    nodeCountTrees=size(self%nodes)
    call allocateArray(nodeIncomplete         ,[nodeCountTrees])
    call allocateArray(nodeSelfIndices        ,[nodeCountTrees])
    call allocateArray(nodeIndexRanks         ,[nodeCountTrees])
    call allocateArray(nodeDescendentLocations,[nodeCountTrees])
    call allocateArray(nodeTreeIndices        ,[nodeCountTrees])
    nodeSelfIndices=self%nodes%nodeIndex
    nodeTreeIndices=-1
    nodeIndexRanks =sortIndex(nodeSelfIndices)
    nodeIncomplete =.false.
    ! Read descendent information.
    call displayIndent ('Reading merger tree data',verbosityLevelWorking)
    mergerTrees=self%file%openGroup("MergerTree")
    call mergerTrees%readDataset  ("HaloID"         ,mergerTreeHaloIndices      )
    call mergerTrees%readDataset  ("DescendantIndex",mergerTreeDescendentIndices)
    call mergerTrees%readAttribute("HaloIndexOffset",haloIndexOffset            )
    call mergerTrees%close()
    call displayUnindent     ('done',verbosityLevelWorking)
    call displayIndent ('Assigning descendant indices',verbosityLevelWorking)
    do i=1,size(mergerTreeHaloIndices)
       call displayCounter(int(100.0d0*dble(i-1)/dble(size(mergerTreeHaloIndices))),i==1,verbosityLevelWorking)
       iHalo=searchIndexed(nodeSelfIndices,nodeIndexRanks,mergerTreeHaloIndices(i))
       if (self%nodes(iHalo)%nodeIndex /= mergerTreeHaloIndices(i)) call Galacticus_Error_Report('mismatch in halo ID lookup'//{introspection:location})
       if (mergerTreeDescendentIndices(i) < 0) then
          self%nodes(iHalo)%descendentIndex=-1
       else
          self%nodes(iHalo)%descendentIndex=mergerTreeHaloIndices(mergerTreeDescendentIndices(i)+1-haloIndexOffset)
       end if
    end do
    call deallocateArray(mergerTreeHaloIndices      )
    call deallocateArray(mergerTreeDescendentIndices)
    call displayCounterClear(       verbosityLevelWorking)
    call displayUnindent     ('done',verbosityLevelWorking)
    call displayIndent ('Locating descendants',verbosityLevelWorking)
    do i=1,nodeCountTrees
       call displayCounter(int(100.0d0*dble(i-1)/dble(nodeCountTrees)),i==1,verbosityLevelWorking)
       if (self%nodes(i)%descendentIndex > 0) then
          iProgenitor=searchIndexed(nodeSelfIndices,nodeIndexRanks,self%nodes(i)%descendentIndex)
          nodeDescendentLocations(i)=iProgenitor
          if (self%nodes(nodeDescendentLocations(i))%nodeIndex /= self%nodes(i)%descendentIndex) call Galacticus_Error_Report('mismatch in descendant ID lookup'//{introspection:location})
       else
          nodeDescendentLocations(i)=-1
       end if
    end do
    call displayCounterClear(       verbosityLevelWorking)
    call displayUnindent     ('done',verbosityLevelWorking)
    ! Indicate that tree indices were not assigned.
    treeIndicesAssigned    =.false.
    ! Indicate that branch jump checks are quired.
    branchJumpCheckRequired=.true.
    ! Clean up display.
    call displayUnindent     ('done',verbosityLevelWorking)
    return

  contains

    function decodeUnits(unitString)
      !% Decode a textual unit definition and construct an importer units object from it.
      use :: Display                         , only : displayMagenta         , displayReset
      use :: Galacticus_Error                , only : Galacticus_Error_Report, Galacticus_Warn
      use :: Numerical_Constants_Astronomical, only : kiloParsec             , massSolar
      use :: Numerical_Constants_Prefixes    , only : kilo
      implicit none
      type     (importerUnits)                :: decodeUnits
      character(len=*        ), intent(in   ) :: unitString
      character(len=32       )                :: unitWork

      ! Check for trailing question mark.
      if (unitString(len_trim(unitString):len_trim(unitString)) == "?") then
         unitWork=trim(unitString(1:len_trim(unitString)-1))
         call Galacticus_Warn(displayMagenta()//'WARNING:'//displayReset()//' file seems to be unsure about units "'//trim(unitString)//'"')
      else
         unitWork=trim(unitString)
      end if
      ! Check for unknown units.
      if (trim(unitWork) == "") then
         ! Return an uninitialize units object.
         decodeUnits=importerUnits(.false.,0.0d0,0,0)
      else
         ! Decode the units.
         select case (trim(unitWork))
         case ('Msun')
            decodeUnits=importerUnits(.true.,massSolar ,-1, 0) ! Assume h^-1.
         case ('km/s')
            decodeUnits=importerUnits(.true.,kilo      , 0, 0) ! Assume physical.
         case ('kpc' )
            decodeUnits=importerUnits(.true.,kiloParsec,-1,+1) ! Assume h^-1 and comoving.
         case default
            call Galacticus_Error_Report('unknown unit specifier'//{introspection:location})
         end select
      end if
      return
    end function decodeUnits

  end subroutine sussingHDF5Load
