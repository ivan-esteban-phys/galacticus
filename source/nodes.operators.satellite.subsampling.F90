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

  !% Implements a node operator class that does a subsampling of satellites based on their properties at infall.

  !# <nodeOperator name="nodeOperatorSatelliteSubsampling">
  !#  <description>
  !#   A node operator class that does a subsampling of satellites based on their properties at infall. The sampling
  !#   function has a form of
  !#   \begin{equation}
  !#   f = \alpha (M_{\mathrm{sat}}/M_0)^\beta,
  !#   \end{equation}
  !#   where $M_\mathrm{sat}$ is the satellite's mass, $M_0$ is a reference mass scale. The normalization $\alpha$
  !#   and slope parameter $\beta$ can be set in the paremter file. A threshold on satellite's mass, infall time,
  !#   and pericenter distance when subsampling is done can also be set to avoid undersampling satellites that are 
  !#   less abundant, e.g. satellites with large mass, or in plunging orbit.
  !#  </description>
  !# </nodeOperator>
  type, extends(nodeOperatorClass) :: nodeOperatorSatelliteSubsampling
     !% A node operator class that does a subsampling of satellites based on their properties at infall.
     private
     double precision :: samplingMassThreshold        , samplingInfallTimeThreshold, &
          &              samplingPericenterThreshold  , samplingFunctionSlope      , &
          &              samplingFunctionNormalization
   contains
     !# <methods>
     !#   <method description="Compute the sampling rate." method="samplingRate" />
     !# </methods>
     final     ::                 satelliteSubsamplingDestructor
     procedure :: nodesMerge   => satelliteSubsamplingNodesMerge
     procedure :: samplingRate => satelliteSubsamplingSamplingRate
  end type nodeOperatorSatelliteSubsampling
  
  interface nodeOperatorSatelliteSubsampling
     !% Constructors for the {\normalfont \ttfamily satelliteSubsampling} node operator class.
     module procedure satelliteSubsamplingConstructorParameters
     module procedure satelliteSubsamplingConstructorInternal
  end interface nodeOperatorSatelliteSubsampling

contains

  function satelliteSubsamplingConstructorParameters(parameters) result(self)
    !% Constructor for the {\normalfont \ttfamily satelliteSubsampling} node operator class which takes a parameter set as input.
    use :: Input_Parameters, only : inputParameters
    implicit none
    type            (nodeOperatorSatelliteSubsampling)                :: self
    type            (inputParameters                 ), intent(inout) :: parameters
    double precision                                                  :: samplingMassThreshold        , samplingInfallTimeThreshold, &
         &                                                               samplingPericenterThreshold  , samplingFunctionSlope      , &
         &                                                               samplingFunctionNormalization

    !# <inputParameter>
    !#   <name>samplingMassThreshold</name>
    !#   <defaultValue>0.0d0</defaultValue>
    !#   <description>Mass threshold below which satellites are subsampled.</description>
    !#   <source>parameters</source>
    !# </inputParameter>
    !# <inputParameter>
    !#   <name>samplingInfallTimeThreshold</name>
    !#   <defaultValue>huge(0.0d0)</defaultValue>
    !#   <description>Infall time threshold below which satellites are subsampled.</description>
    !#   <source>parameters</source>
    !# </inputParameter>
    !# <inputParameter>
    !#   <name>samplingPericenterThreshold</name>
    !#   <defaultValue>0.0d0</defaultValue>
    !#   <description>Pericenter distance threshold above which satellites are subsampled in units of the distance to the host halo at infall.</description>
    !#   <source>parameters</source>
    !# </inputParameter>
    !# <inputParameter>
    !#   <name>samplingFunctionSlope</name>
    !#   <defaultValue>0.0d0</defaultValue>
    !#   <description>Slope of the sampling function.</description>
    !#   <source>parameters</source>
    !# </inputParameter>
    !# <inputParameter>
    !#   <name>samplingFunctionNormalization</name>
    !#   <defaultValue>1.0d0</defaultValue>
    !#   <description>Normalization of the sampling function.</description>
    !#   <source>parameters</source>
    !# </inputParameter>
    self=nodeOperatorSatelliteSubsampling(samplingMassThreshold,samplingInfallTimeThreshold,samplingPericenterThreshold,samplingFunctionSlope,samplingFunctionNormalization)
    !# <inputParametersValidate source="parameters"/>
    return
  end function satelliteSubsamplingConstructorParameters

  function satelliteSubsamplingConstructorInternal(samplingMassThreshold,samplingInfallTimeThreshold,samplingPericenterThreshold,samplingFunctionSlope,samplingFunctionNormalization) result(self)
    !% Internal constructor for the {\normalfont \ttfamily satelliteSubsampling} node operator class.
    implicit none
    type            (nodeOperatorSatelliteSubsampling)                :: self
    double precision                                  , intent(in   ) :: samplingMassThreshold        , samplingInfallTimeThreshold, &
         &                                                               samplingPericenterThreshold  , samplingFunctionSlope      , &
         &                                                               samplingFunctionNormalization
    !# <constructorAssign variables="samplingMassThreshold,samplingInfallTimeThreshold,samplingPericenterThreshold,samplingFunctionSlope,samplingFunctionNormalization"/>
    
    return
  end function satelliteSubsamplingConstructorInternal

  subroutine satelliteSubsamplingDestructor(self)
    !% Destructor for the {\normalfont \ttfamily satelliteSubsampling} node operator class.
    implicit none
    type(nodeOperatorSatelliteSubsampling), intent(inout) :: self
    !$GLC attributes unused :: self

    return
  end subroutine satelliteSubsamplingDestructor

  subroutine satelliteSubsamplingNodesMerge(self,node)
    !% Does a subsampling of satellites based on their infall mass and orbital parameters.
    use :: Galacticus_Nodes   , only : nodeComponentSatellite
    use :: Merger_Tree_Walkers, only : mergerTreeWalkerAllNodesBranch
    implicit none
    class           (nodeOperatorSatelliteSubsampling), intent(inout) :: self
    type            (treeNode                        ), intent(inout) :: node
    class           (nodeComponentSatellite          ), pointer       :: satellite       , satelliteHost
    type            (mergerTreeWalkerAllNodesBranch  )                :: treeWalker
    type            (treeNode                        ), pointer       :: workNode        , hostNode
    double precision                                                  :: sample          , samplingRate   , &
         &                                                               sampleWeight    , sampleWeightNew, &
         &                                                               sampleWeightHost
    
    satellite => node%satellite()
    ! Compute the sampling rate.
    samplingRate = self%samplingRate(node)
    if (samplingRate == 1.0d0) then
       return
    else
       ! Do subsampling.
       sample    =node%hostTree%randomNumberGenerator_%uniformSample()
       treeWalker=mergerTreeWalkerAllNodesBranch(node)
       if (sample >= samplingRate) then
          ! Remove this satellite and its own satellites by setting their destruction time to zero.
          call satellite%destructionTimeSet(0.0d0)
          do while (treeWalker%next(workNode))
             satellite => workNode%satellite()
             call satellite%destructionTimeSet(0.0d0)
          end do
       else
          ! Adjust the weights of this satellite. The weights of satellites this satellite contains
          ! are changed accordingly.
          sampleWeightNew=1.0d0/samplingRate
          call satellite%subsamplingWeightSet(sampleWeightNew)
          do while (treeWalker%next(workNode))
             hostNode         => workNode%mergesWith()
             satellite        => workNode%satellite ()
             satelliteHost    => hostNode%satellite ()
             sampleWeight     =  satellite    %subsamplingWeight()
             sampleWeightHost =  satelliteHost%subsamplingWeight()
             sampleWeightNew  =  sampleWeight*sampleWeightHost
             call satellite%subsamplingWeightSet(sampleWeightNew)
          end do
       end if
    end if
    return
  end subroutine satelliteSubsamplingNodesMerge
  
  double precision function satelliteSubsamplingSamplingRate(self,node)
    !% Compute the sampling rate for a satellite based on its properties at infall.
    use :: Galacticus_Nodes, only : nodeComponentBasic, nodeComponentSatellite
    use :: Kepler_Orbits   , only : keplerOrbit
    implicit none
    class           (nodeOperatorSatelliteSubsampling), intent(inout) :: self
    type            (treeNode                        ), intent(inout) :: node
    class           (nodeComponentBasic              ), pointer       :: basic
    class           (nodeComponentSatellite          ), pointer       :: satellite
    type            (keplerOrbit                     )                :: orbit
    double precision                                                  :: infallMass    , infallTime        , &
         &                                                               infallDistance, pericenterDistance

    satelliteSubsamplingSamplingRate = 1.0d0
    if (self%samplingMassThreshold > 0.0d0) then
       satellite          => node     %satellite       ()
       basic              => node     %basic           ()
       orbit              =  satellite%virialOrbit     ()
       infallMass         =  basic    %mass            ()
       infallTime         =  basic    %time            ()
       infallDistance     =  orbit    %radius          ()
       pericenterDistance =  orbit    %radiusPericenter()
       if     (                                                                      &
            &   infallMass                        < self%samplingMassThreshold       &
            &  .and.                                                                 &
            &   infallTime                        < self%samplingInfallTimeThreshold &
            &  .and.                                                                 &
            &   pericenterDistance/infallDistance > self%samplingPericenterThreshold &
            & ) then
          satelliteSubsamplingSamplingRate = +self%samplingFunctionNormalization &
               &                             *(                                  &
               &                               +infallMass                       &
               &                               /self%samplingMassThreshold       &
               &                              )**self%samplingFunctionSlope
       end if
    end if
    return
  end function satelliteSubsamplingSamplingRate
