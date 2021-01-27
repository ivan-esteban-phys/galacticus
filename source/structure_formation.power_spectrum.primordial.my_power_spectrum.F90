!! Copyright 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018,
!!           2019, 2020
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

  !% A primordial power spectrum class which provides a broken power-law power spectrum. Partly implemented by Ivan Esteban

  !# <powerSpectrumPrimordial name="powerSpectrumPrimordialMyPowerSpectrum">
  !#  <description>
  !#   Implements a broken power-law primordial power spectrum, possibly with a running index. The primordial power spectrum has the
  !#   form:
  !#   \begin{equation}
  !#     P(k) \propto \begin{cases} k^{n_\mathrm{eff}(k)} &amp; \mathrm{if} \, k \leq k_\mathrm{cut} \\ 
  !#                                k^{n_\mathrm{cut}} &amp; \mathrm{if} \, k \geq k_\mathrm{cut}
  !#    \end{cases}
  !#   \end{equation}
  !#   where
  !#   \begin{equation}
  !#    n_\mathrm{eff}(k) = n_\mathrm{s} + {1\over 2}{\d n \over \d \ln k} \ln \left( {k \over k_\mathrm{ref}} \right),
  !#   \end{equation}
  !#   where $n_\mathrm{s}=${\normalfont \ttfamily [index]} is the power spectrum index at wavenumber
  !#   $k_\mathrm{ref}=${\normalfont \ttfamily [wavenumberReference]} and $\d n / \d \ln k=${\normalfont \ttfamily [running]}
  !#   describes the running of this index with wavenumber.
  !#  </description>
  !# </powerSpectrumPrimordial>
  type, extends(powerSpectrumPrimordialClass) :: powerSpectrumPrimordialMyPowerSpectrum
     !% A power-law primordial power spectrum class.
     private
     double precision :: index              , running, &
          &              wavenumberReference, waveNumberCut, indexCut
   contains
     final     ::                          myPowerSpectrumDestructor
     procedure :: power                 => myPowerSpectrumPower
     procedure :: logarithmicDerivative => myPowerSpectrumLogarithmicDerivative
  end type powerSpectrumPrimordialMyPowerSpectrum

  interface powerSpectrumPrimordialMyPowerSpectrum
     !% Constructors for the ``power-law'' primordial power spectrum class.
     module procedure myPowerSpectrumConstructorParameters
     module procedure myPowerSpectrumConstructorInternal
  end interface powerSpectrumPrimordialMyPowerSpectrum

contains

  function myPowerSpectrumConstructorParameters(parameters)
    !% Constructor for the ``power-law'' primordial power spectrum class which takes a parameter set as input.
    implicit none
    type(powerSpectrumPrimordialMyPowerSpectrum)                :: myPowerSpectrumConstructorParameters
    type(inputParameters                ), intent(inout) :: parameters

    !# <inputParameter>
    !#   <name>index</name>
    !#   <source>parameters</source>
    !#   <variable>myPowerSpectrumConstructorParameters%index</variable>
    !#   <defaultValue>0.9649d0</defaultValue>
    !#   <defaultSource>(\citealt{planck_collaboration_planck_2018}; TT,TE,EE$+$lowE$+$lensing)</defaultSource>
    !#   <description>The index of the power-law primordial power spectrum.</description>
    !# </inputParameter>
    !# <inputParameter>
    !#   <name>running</name>
    !#   <source>parameters</source>
    !#   <variable>myPowerSpectrumConstructorParameters%running</variable>
    !#   <defaultValue>0.0d0</defaultValue>
    !#   <description>The running, $\d n_\mathrm{s} / \d \ln k$, of the power spectrum index.</description>
    !# </inputParameter>
    !# <inputParameter>
    !#   <name>wavenumberReference</name>
    !#   <source>parameters</source>
    !#   <variable>myPowerSpectrumConstructorParameters%wavenumberReference</variable>
    !#   <defaultValue>1.0d0</defaultValue>
    !#   <description>When a running power spectrum index is used, this is the wavenumber at which the index is equal to {\normalfont \ttfamily [index]}.</description>
    !# </inputParameter>
    !# <inputParameter>
    !#   <name>wavenumberCut</name>
    !#   <source>parameters</source>
    !#   <variable>myPowerSpectrumConstructorParameters%wavenumberCut</variable>
    !#   <defaultValue>1.0d0</defaultValue>
    !#   <description>When a running power spectrum index is used, this is the wavenumber at which the index is equal to {\normalfont \ttfamily [index]}.</description>
    !# </inputParameter>
    !# <inputParameter>
    !#   <name>indexCut</name>
    !#   <source>parameters</source>
    !#   <variable>myPowerSpectrumConstructorParameters%indexCut</variable>
    !#   <defaultValue>1.0d0</defaultValue>
    !#   <description>When a running power spectrum index is used, this is the wavenumber at which the index is equal to {\normalfont \ttfamily [index]}.</description>
    !# </inputParameter>
    !# <inputParametersValidate source="parameters"/>
    return
  end function myPowerSpectrumConstructorParameters

  function myPowerSpectrumConstructorInternal(index,running,wavenumberReference,wavenumberCut,indexCut)
    !% Internal constructor for the ``power-law'' primordial power spectrum class.
    implicit none
    type            (powerSpectrumPrimordialMyPowerSpectrum)                :: myPowerSpectrumConstructorInternal
    double precision                                 , intent(in   ) :: index                      , running, &
         &                                                              wavenumberReference, waveNumberCut, indexCut

    myPowerSpectrumConstructorInternal%index              =index
    myPowerSpectrumConstructorInternal%running            =running
    myPowerSpectrumConstructorInternal%wavenumberReference=wavenumberReference
    myPowerSpectrumConstructorInternal%wavenumberCut=wavenumberCut
    myPowerSpectrumConstructorInternal%indexCut=indexCut
    return
  end function myPowerSpectrumConstructorInternal

  elemental subroutine myPowerSpectrumDestructor(self)
    !% Destructor for the ``power-law'' primordial power spectrum class.
    implicit none
    type(powerSpectrumPrimordialMyPowerSpectrum), intent(inout) :: self
    !$GLC attributes unused :: self

    ! Nothing to do.
    return
  end subroutine myPowerSpectrumDestructor

  double precision function myPowerSpectrumPower(self,wavenumber)
    !% Return the primordial power spectrum at the given {\normalfont \ttfamily wavenumber}.
    implicit none
    class           (powerSpectrumPrimordialMyPowerSpectrum), intent(inout) :: self
    double precision                                 , intent(in   ) :: wavenumber

    myPowerSpectrumPower=+(                                  &
         &          +             wavenumber          &
         &          /        self%wavenumberReference &
         &         )**(                               &
         &             +self%index                    &
         &             +0.5d0                         &
         &             *self%running                  &
         &             *log(                          &
         &                  +     wavenumber          &
         &                  /self%wavenumberReference &
         &                 )                          &
         &            )
    
    if (wavenumber > self%wavenumberCut) then !% Enforce continuity of the power spectrum
       myPowerSpectrumPower=+(                                  &
         &          +        self%wavenumberCut       &
         &          /        self%wavenumberReference &
         &         )**(                               &
         &             +self%index                    &
         &             +0.5d0                         &
         &             *self%running                  &
         &             *log(                          &
         &                  +self%wavenumberCut       &
         &                  /self%wavenumberReference &
         &                 )                          &
         &            )*(                             &
         &          +             wavenumber          &
         &          /        self%wavenumberCut       &
         &         )**(                               &
         &             +self%indexCut                 &
         &            )
    end if

    return
  end function myPowerSpectrumPower

  double precision function myPowerSpectrumLogarithmicDerivative(self,wavenumber)
    !% Return the logarithmic derivative of the primordial power spectrum at the given {\normalfont \ttfamily wavenumber}.
    implicit none
    class           (powerSpectrumPrimordialMyPowerSpectrum), intent(inout) :: self
    double precision                                 , intent(in   ) :: wavenumber

    myPowerSpectrumLogarithmicDerivative=+self%index                    &
         &                        +self%running                  &
         &                        *log(                          &
         &                             +     wavenumber          &
         &                             /self%wavenumberReference &
         &                            )

    if (wavenumber > self%wavenumberCut) then
       myPowerSpectrumLogarithmicDerivative=+self%indexCut
    end if

    return
  end function myPowerSpectrumLogarithmicDerivative
