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

  !% Implements a merger tree operator class which dumps pre-evolution tree structure to the output file.

  use :: IO_HDF5                 , only : hdf5Object
  use :: Node_Property_Extractors, only : nodePropertyExtractorClass
  
  !# <mergerTreeOperator name="mergerTreeOperatorOutputStructure">
  !#  <description>
  !#   A merger tree operator class which dumps pre-evolution tree structure to the output file. The node properties to be
  !#   included in the dump are controlled by a \refClass{nodePropertyExtractorClass} object provided to this class.  Structures are
  !#   written to a new group, {\normalfont \ttfamily mergerTreeStructures}, in the \glc\ output file. This group will contain
  !#   groups called {\normalfont \ttfamily mergerTreeN} where {\normalfont \ttfamily N} is the merger tree index. Each such group
  !#   will contain datasets corresponding to all extracted properties.
  !#  </description>
  !# </mergerTreeOperator>
  type, extends(mergerTreeOperatorClass) :: mergerTreeOperatorOutputStructure
     !% A merger tree operator class which dumps pre-evolution tree structure to the output file.
     private
     class(nodePropertyExtractorClass), pointer :: nodePropertyExtractor_
     type (hdf5Object                )          :: outputGroup
   contains
     final     ::                        outputStructureDestructor
     procedure :: operatePreEvolution => outputStructureOperatePreEvolution
     procedure :: finalize            => outputStructureFinalize
  end type mergerTreeOperatorOutputStructure

  interface mergerTreeOperatorOutputStructure
     !% Constructors for the {\normalfont \ttfamily outputStructure} merger tree operator class.
     module procedure outputStructureConstructorParameters
     module procedure outputStructureConstructorInternal
  end interface mergerTreeOperatorOutputStructure

contains

  function outputStructureConstructorParameters(parameters) result(self)
    !% Constructor for the {\normalfont \ttfamily outputStructure} merger tree operator class which takes a parameter set as
    !% input.
    use :: Input_Parameters, only : inputParameters
    implicit none
    type (mergerTreeOperatorOutputStructure)                :: self
    type (inputParameters                  ), intent(inout) :: parameters
    class(nodePropertyExtractorClass       ), pointer       :: nodePropertyExtractor_

    !# <objectBuilder class="nodePropertyExtractor" name="nodePropertyExtractor_" source="parameters"/>
    self=outputStructureConstructorInternal(nodePropertyExtractor_)
    !# <inputParametersValidate source="parameters"/>
    !# <objectDestructor name="nodePropertyExtractor_"/>
    return
  end function outputStructureConstructorParameters

  function outputStructureConstructorInternal(nodePropertyExtractor_) result(self)
    !% Internal constructor for the {\normalfont \ttfamily outputStructure} merger tree operator class.
    implicit none
    type (mergerTreeOperatorOutputStructure)                        :: self
    class(nodePropertyExtractorClass       ), intent(in   ), target :: nodePropertyExtractor_
    !# <constructorAssign variables="*nodePropertyExtractor_"/>

    return
  end function outputStructureConstructorInternal

  subroutine outputStructureDestructor(self)
    !% Destructor for the {\normalfont \ttfamily outputStructure} merger tree operator function class.
    implicit none
    type(mergerTreeOperatorOutputStructure), intent(inout) :: self

    call self%finalize()
    !# <objectDestructor name="self%nodePropertyExtractor_"/>
    return
  end subroutine outputStructureDestructor
  
  subroutine outputStructureOperatePreEvolution(self,tree)
    !% Output the structure of {\normalfont \ttfamily tree}.
    use    :: Galacticus_Error        , only : Galacticus_Error_Report
    use    :: Galacticus_HDF5         , only : galacticusOutputFile
    use    :: Galacticus_Nodes        , only : nodeComponentBasic
    !$ use :: IO_HDF5                 , only : hdf5Access
    use    :: Kind_Numbers            , only : kind_int8
    use    :: Merger_Tree_Walkers     , only : mergerTreeWalkerIsolatedNodes
    use    :: Node_Property_Extractors, only : elementTypeDouble            , elementTypeInteger       , nodePropertyExtractorIntegerScalar, nodePropertyExtractorIntegerTuple, &
         &                                     nodePropertyExtractorMulti   , nodePropertyExtractorNull, nodePropertyExtractorScalar       , nodePropertyExtractorTuple
    use :: String_Handling, only : operator(//)
    implicit none
    class           (mergerTreeOperatorOutputStructure), intent(inout) , target      :: self
    type            (mergerTree                       ), intent(inout) , target      :: tree
    type            (mergerTree                       )                , pointer     :: treeCurrent
    type            (treeNode                         )                , pointer     :: node
    class           (nodeComponentBasic               )                , pointer     :: basic
    double precision                                   , dimension(:,:), allocatable :: propertiesDouble
    integer         (kind_int8                        ), dimension(:,:), allocatable :: propertiesInteger
    type            (varying_string                   ), dimension(  :), allocatable :: namesDouble          , namesInteger          , &
         &                                                                              descriptionsDouble   , descriptionsInteger
    double precision                                   , dimension(  :), allocatable :: unitsInSIDouble      , unitsInSIInteger
    type            (mergerTreeWalkerIsolatedNodes    )                              :: treeWalker
    integer                                                                          :: countNodes           , countPropertiesInteger, &
         &                                                                              countPropertiesDouble, i
    type            (hdf5Object                       )                              :: treeGroup            , dataset

    !$ call hdf5Access%set  ()
    if (.not.self%outputGroup%isOpen()) self%outputGroup=galacticusOutputFile%openGroup("mergerTreeStructures","Pre-evolution structures of merger trees.")
    !$ call hdf5Access%unset()
    treeCurrent => tree
    do while (associated(treeCurrent))
       !$ call hdf5Access%set()
       treeGroup=self%outputGroup%openGroup(char(var_str('tree')//treeCurrent%index),'Pre-evolution structure of merger tree.')
       !$ call hdf5Access%unset()
       ! Walk the tree to count total number of nodes.
       countNodes =0
       treeWalker=mergerTreeWalkerIsolatedNodes(treeCurrent)
       do while (treeWalker%next(node))
          countNodes=countNodes+1
       end do
       ! Determine the number of properties for output.
       basic                  => treeCurrent%baseNode%basic()
       countPropertiesDouble  =  0
       countPropertiesInteger =  0
       select type (extractor_ => self%nodePropertyExtractor_)
       type is (nodePropertyExtractorNull)
          ! Null extractor - pointless.
          call Galacticus_Error_Report('null extractor is pointless'//{introspection:location})
       class is (nodePropertyExtractorScalar       )
          ! Scalar property extractor.
          countPropertiesDouble=1
       class is (nodePropertyExtractorTuple        )
          ! Tuple property extractor.
          countPropertiesDouble =extractor_%elementCount(                   basic%time())
       class is (nodePropertyExtractorIntegerScalar)
          ! Integer scalar property extractor.
          countPropertiesInteger=1
       class is (nodePropertyExtractorIntegerTuple )
          ! Integer tuple property extractor.
          countPropertiesInteger=extractor_%elementCount(                   basic%time())
       class is (nodePropertyExtractorMulti        )
          ! Multi property extractor.
          countPropertiesDouble =extractor_%elementCount(elementTypeDouble ,basic%time())
          countPropertiesInteger=extractor_%elementCount(elementTypeInteger,basic%time())
       class default
          call Galacticus_Error_Report('unsupported property extractor class'//{introspection:location})
       end select
       ! Allocate storage for the properties.
       allocate(propertiesDouble   (countNodes,countPropertiesDouble ))
       allocate(propertiesInteger  (countNodes,countPropertiesInteger))
       allocate(namesDouble        (           countPropertiesDouble ))
       allocate(namesInteger       (           countPropertiesInteger))
       allocate(descriptionsDouble (           countPropertiesDouble ))
       allocate(descriptionsInteger(           countPropertiesInteger))
       allocate(unitsInSIDouble    (           countPropertiesDouble ))
       allocate(unitsInSIInteger   (           countPropertiesInteger))
       ! Retrieve property names, descriptions, and units.
       select type (extractor_ => self%nodePropertyExtractor_)
       class is (nodePropertyExtractorScalar       )
          ! Scalar property extractor.
          namesDouble        (1)=extractor_%name        (                               )
          descriptionsDouble (1)=extractor_%description (                               )
          unitsInSIDouble    (1)=extractor_%unitsInSI   (                               )
       class is (nodePropertyExtractorTuple        )
          ! Tuple property extractor.
          namesDouble        (:)=extractor_%names       (                   basic%time())
          descriptionsDouble (:)=extractor_%descriptions(                   basic%time())
          unitsInSIDouble    (:)=extractor_%unitsInSI   (                   basic%time())
       class is (nodePropertyExtractorIntegerScalar)
          ! Integer scalar property extractor.
          namesInteger       (1)=extractor_%name        (                               )
          descriptionsInteger(1)=extractor_%description (                               )
          unitsInSIInteger   (1)=extractor_%unitsInSI   (                               )          
       class is (nodePropertyExtractorIntegerTuple )
          ! Integer tuple property extractor.
          namesInteger       (:)=extractor_%names       (                   basic%time())
          descriptionsInteger(:)=extractor_%descriptions(                   basic%time())
          unitsInSIInteger   (:)=extractor_%unitsInSI   (                   basic%time())
       class is (nodePropertyExtractorMulti        )
          ! Multi property extractor.
          namesDouble        (:)=extractor_%names       (elementTypeDouble ,basic%time())
          descriptionsDouble (:)=extractor_%descriptions(elementTypeDouble ,basic%time())
          unitsInSIDouble    (:)=extractor_%unitsInSI   (elementTypeDouble ,basic%time())
          namesInteger       (:)=extractor_%names       (elementTypeInteger,basic%time())
          descriptionsInteger(:)=extractor_%descriptions(elementTypeInteger,basic%time())
          unitsInSIInteger   (:)=extractor_%unitsInSI   (elementTypeInteger,basic%time())
       end select
       ! Walk the tree again accumulating properties for output to arrays.
       countNodes =0
       treeWalker=mergerTreeWalkerIsolatedNodes(treeCurrent)
       do while (treeWalker%next(node))
          countNodes=countNodes+1
          select type (extractor_ => self%nodePropertyExtractor_)
          class is (nodePropertyExtractorScalar       )
             ! Scalar property extractor.
             propertiesDouble (countNodes,1)=extractor_%extract       (node             )
          class is (nodePropertyExtractorTuple        )
             ! Tuple property extractor.
             if     ( countPropertiesDouble  /= extractor_%elementCount(                   basic%time())) &
                  & call Galacticus_Error_Report('unsupported change in number of properties'//{introspection:location})
             propertiesDouble (countNodes,:)=extractor_%extract       (node,basic%time())
         class is (nodePropertyExtractorIntegerScalar)
             ! Integer scalar property extractor.
             propertiesInteger(countNodes,1)=extractor_%extract       (node,basic%time())
          class is (nodePropertyExtractorIntegerTuple )
             ! Integer tuple property extractor.
             if     ( countPropertiesInteger /= extractor_%elementCount(                   basic%time())) &
                  & call Galacticus_Error_Report('unsupported change in number of properties'//{introspection:location})
             propertiesInteger(countNodes,:)=extractor_%extract       (node,basic%time())
          class is (nodePropertyExtractorMulti        )
             ! Multi property extractor.
             if     (                                                                                     &
                  &   countPropertiesDouble  /= extractor_%elementCount(elementTypeDouble ,basic%time())  &
                  &  .or.                                                                                 &
                  &   countPropertiesInteger /= extractor_%elementCount(elementTypeInteger,basic%time())  &
                  & ) call Galacticus_Error_Report('unsupported change in number of properties'//{introspection:location})
             propertiesDouble (countNodes,:)=extractor_%extractDouble (node,basic%time())
             propertiesInteger(countNodes,:)=extractor_%extractInteger(node,basic%time())
          end select
       end do
       ! Output all accumulated properties.
       !$ call hdf5Access%set  ()
       do i=1,countPropertiesDouble
          call treeGroup%writeDataset  (propertiesDouble (:,i),char(namesDouble (i)),char(descriptionsDouble (i)),datasetReturned=dataset)
          call dataset  %writeAttribute(unitsInSIDouble  (  i),'unitsInSI'                                                               )
          call dataset  %close         (                                                                                                 )
       end do
       do i=1,countPropertiesInteger
          call treeGroup%writeDataset  (propertiesInteger(:,i),char(namesInteger(i)),char(descriptionsInteger(i)),datasetReturned=dataset)
          call dataset  %writeAttribute(unitsInSIInteger (  i),'unitsInSI'                                                               )
          call dataset  %close         (                                                                                                 )
       end do
       !$ call hdf5Access%unset()
       call treeGroup%close()
       ! Free workspace.
       deallocate(propertiesDouble   )
       deallocate(propertiesInteger  )
       deallocate(namesDouble        )
       deallocate(namesInteger       )
       deallocate(descriptionsDouble )
       deallocate(descriptionsInteger)
       deallocate(unitsInSIDouble    )
       deallocate(unitsInSIInteger   )
       ! Move to the next tree.
       treeCurrent => treeCurrent%nextTree
    end do
    return
  end subroutine outputStructureOperatePreEvolution

  subroutine outputStructureFinalize(self)
    !% Close the merger tree structure group.
    !$ use :: IO_HDF5, only : hdf5Access
    implicit none
    class(mergerTreeOperatorOutputStructure), intent(inout) :: self

    !$ call  hdf5Access%set  ()
    if (self%outputGroup%isOpen()) call self%outputGroup%close()
    !$ call  hdf5Access%unset()
    return
  end subroutine outputStructureFinalize
