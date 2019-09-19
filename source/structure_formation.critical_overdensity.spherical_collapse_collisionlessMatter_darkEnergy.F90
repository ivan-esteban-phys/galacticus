!! Copyright 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018,
!!           2019
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

  !% An implementation of critical overdensity for collapse based on spherical collapse in a
  !% matter plus dark energy universe.

  !# <criticalOverdensity name="criticalOverdensitySphericalCollapseMatterDE">
  !#  <description>Critical overdensity for collapse based on the spherical collapse in a matter plus dark energy universe.</description>
  !# </criticalOverdensity>
  type, extends(criticalOverdensitySphericalCollapseMatterLambda) :: criticalOverdensitySphericalCollapseMatterDE
     !% A dark matter halo virial density contrast class based on spherical collapse in a matter plus dark energy universe.
     private
  end type criticalOverdensitySphericalCollapseMatterDE

  interface criticalOverdensitySphericalCollapseMatterDE
     !% Constructors for the {\normalfont \ttfamily sphericalCollapseMatterDE} critical overdensity for collapse class.
     module procedure sphericalCollapseMatterDEConstructorParameters
     module procedure sphericalCollapseMatterDEConstructorInternal
  end interface criticalOverdensitySphericalCollapseMatterDE

contains

  function sphericalCollapseMatterDEConstructorParameters(parameters) result(self)
    !% Constructor for the {\normalfont \ttfamily sphericalCollapseMatterDE} critical overdensity class
    !% which takes a parameter set as input.
    use Input_Parameters
    use Dark_Matter_Particles
    use Galacticus_Error
    implicit none
    type            (criticalOverdensitySphericalCollapseMatterDE)                :: self
    type            (inputParameters                             ), intent(inout) :: parameters
    class           (cosmologyFunctionsClass                     ), pointer       :: cosmologyFunctions_    
    class           (linearGrowthClass                           ), pointer       :: linearGrowth_    
    class           (cosmologicalMassVarianceClass               ), pointer       :: cosmologicalMassVariance_
    class           (darkMatterParticleClass                     ), pointer       :: darkMatterParticle_
    double precision                                                              :: normalization
    logical                                                                       :: tableStore

    !# <inputParameter>
    !#   <name>normalization</name>
    !#   <source>parameters</source>
    !#   <defaultValue>1.0d0</defaultValue>
    !#   <description>A normalizing factor to be applied to the critical overdensity.</description>
    !#   <type>real</type>
    !#   <cardinality>0..1</cardinality>
    !# </inputParameter>
    !# <inputParameter>
    !#   <name>tableStore</name>
    !#   <source>parameters</source>
    !#   <defaultValue>.true.</defaultValue>
    !#   <description>If true, store/restore the tabulated solution to/from file when possible.</description>
    !#   <type>boolean</type>
    !#   <cardinality>0..1</cardinality>
    !# </inputParameter>
    !# <objectBuilder class="linearGrowth"             name="linearGrowth_"             source="parameters"/>
    !# <objectBuilder class="cosmologyFunctions"       name="cosmologyFunctions_"       source="parameters"/>
    !# <objectBuilder class="cosmologicalMassVariance" name="cosmologicalMassVariance_" source="parameters"/>
    !# <objectBuilder class="darkMatterParticle"       name="darkMatterParticle_"       source="parameters"/>
    self=criticalOverdensitySphericalCollapseMatterDE(linearGrowth_,cosmologyFunctions_,cosmologicalMassVariance_,darkMatterParticle_,tableStore,normalization)
    !# <inputParametersValidate source="parameters"/>
    !# <objectDestructor name="linearGrowth_"            />
    !# <objectDestructor name="cosmologyFunctions_"      />
    !# <objectDestructor name="cosmologicalMassVariance_"/>
    !# <objectDestructor name="darkMatterParticle_"      />
    return
  end function sphericalCollapseMatterDEConstructorParameters

  function sphericalCollapseMatterDEConstructorInternal(linearGrowth_,cosmologyFunctions_,cosmologicalMassVariance_,darkMatterParticle_,tableStore,normalization) result(self)
    !% Internal constructor for the {\normalfont \ttfamily sphericalCollapseMatterDE} critical overdensity class.
    use Dark_Matter_Particles
    use Galacticus_Error
    use Spherical_Collapse_Solvers, only : sphericalCollapseSolverMatterDarkEnergy, matterDarkEnergyFixedAtUndefined
    implicit none
    type            (criticalOverdensitySphericalCollapseMatterDE)                          :: self
    class           (cosmologyFunctionsClass                     ), target  , intent(in   ) :: cosmologyFunctions_    
    class           (linearGrowthClass                           ), target  , intent(in   ) :: linearGrowth_    
    class           (cosmologicalMassVarianceClass               ), target  , intent(in   ) :: cosmologicalMassVariance_
    class           (darkMatterParticleClass                     ), target  , intent(in   ) :: darkMatterParticle_
    logical                                                                 , intent(in   ) :: tableStore
    double precision                                              , optional, intent(in   ) :: normalization
    !# <optionalArgument name="normalization" defaultsTo="1.0d0" />
    !# <constructorAssign variables="*linearGrowth_, *cosmologyFunctions_, *cosmologicalMassVariance_, *darkMatterParticle_, normalization, tableStore"/>

    self%tableInitialized=.false.
    allocate(sphericalCollapseSolverMatterDarkEnergy :: self%sphericalCollapseSolver_)
    select type (sphericalCollapseSolver_ => self%sphericalCollapseSolver_)
    type is (sphericalCollapseSolverMatterDarkEnergy)
       !# <referenceConstruct isResult="yes" object="sphericalCollapseSolver_" constructor="sphericalCollapseSolverMatterDarkEnergy(matterDarkEnergyFixedAtUndefined,self%cosmologyFunctions_,self%linearGrowth_)"/>
    end select
    ! Require that the dark matter be cold dark matter.
    select type (darkMatterParticle_)
    class is (darkMatterParticleCDM)
       ! Cold dark matter particle - this is as expected.
    class default
       call Galacticus_Error_Report('critical overdensity expects a cold dark matter particle'//{introspection:location})
    end select
    return
  end function sphericalCollapseMatterDEConstructorInternal