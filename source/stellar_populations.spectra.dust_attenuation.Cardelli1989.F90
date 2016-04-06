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

  !% Implements calculations of attenuation of stellar spectra using the model of \cite{cardelli_relationship_1989}.
  
  !# <stellarSpectraDustAttenuation name="stellarSpectraDustAttenuationCardelli1989">
  !#  <description>Returns the dust attenuation of stellar spectra according to the model of \cite{cardelli_relationship_1989}.</description>
  !# </stellarSpectraDustAttenuation>

  type, extends(stellarSpectraDustAttenuationClass) :: stellarSpectraDustAttenuationCardelli1989
     !% A class implementing calculations of attenuation of stellar spectra using the model of \cite{cardelli_relationship_1989}.
     private
     double precision :: Rv
   contains
     !@ <objectMethods>
     !@   <object>stellarSpectraDustAttenuationCardelli1989</object>
     !@   <objectMethod>
     !@     <method>a</method>
     !@     <description>Return fitting function $a(x)$ for the dust attenuation model of \cite{cardelli_relationship_1989}.</description>
     !@     <type>\doublezero</type>
     !@     <arguments>\doublezero\ x\argin</arguments>
     !@   </objectMethod>
     !@   <objectMethod>
     !@     <method>b</method>
     !@     <description>Return fitting function $b(x)$ for the dust attenuation model of \cite{cardelli_relationship_1989}.</description>
     !@     <type>\doublezero</type>
     !@     <arguments>\doublezero\ x\argin</arguments>
     !@   </objectMethod>
     !@ </objectMethods>
     procedure :: attenuation => cardelli1989Attenuation
     procedure :: a           => cardelli1989A
     procedure :: b           => cardelli1989B
  end type stellarSpectraDustAttenuationCardelli1989

  interface stellarSpectraDustAttenuationCardelli1989
     !% Constructors for the ``cardelli1989'' stellar spectra dust attenuation class.
     module procedure cardelli1989DefaultConstructor
     module procedure cardelli1989Constructor
  end interface stellarSpectraDustAttenuationCardelli1989

contains

  function cardelli1989DefaultConstructor()
    !% Default constructor for the ``cardelli1989'' stellar spectra dust attenuation class.
    implicit none
    type            (stellarSpectraDustAttenuationCardelli1989)            :: cardelli1989DefaultConstructor
    ! Standard value of Rv.
    double precision                                           , parameter :: Rv                            =3.1d0

    cardelli1989DefaultConstructor=cardelli1989Constructor(Rv)
    return
  end function cardelli1989DefaultConstructor

  function cardelli1989Constructor(Rv)
    !% Constructor for the ``cardelli1989'' stellar spectra dust attenuation class.
    implicit none
    type            (stellarSpectraDustAttenuationCardelli1989)                :: cardelli1989Constructor
    double precision                                           , intent(in   ) :: Rv

    cardelli1989Constructor%Rv=Rv
    return
  end function cardelli1989Constructor

  double precision function cardelli1989Attenuation(self,wavelength,age,vBandAttenuation)
    !% Return attenuation of stellar spectra according to the model of \cite{cardelli_relationship_1989}.
    use Numerical_Constants_Units
    implicit none
    class           (stellarSpectraDustAttenuationCardelli1989), intent(inout) :: self
    double precision                                           , intent(in   ) :: wavelength      , age, &
         &                                                                        vBandAttenuation
    double precision                                                           :: x

    x                      =1.0d0/(wavelength/angstromsPerMicron)
    cardelli1989Attenuation=vBandAttenuation*(self%a(x)+self%b(x)/self%Rv) ! Eqn. (1) of Cardelli et al.
    return
  end function cardelli1989Attenuation

  double precision function cardelli1989A(self,x)
    !% Return fitting function $a(x)$ for the dust attenuation model of \cite{cardelli_relationship_1989}.
    implicit none
    class           (stellarSpectraDustAttenuationCardelli1989), intent(inout) :: self
    double precision                                           , intent(in   ) :: x
    double precision                                                           :: y   , Fa

    cardelli1989A=0.0d0
    if      (x >= 0.3d0 .and. x < 1.1d0) then
       cardelli1989A=+0.57400d0*x**1.61d0
    else if (x >= 1.1d0 .and. x < 3.3d0) then
       y=x-1.82d0
       cardelli1989A=+1.00000d0          &
            &        +0.17699d0*y        &
            &        -0.50447d0*y**2     &
            &        -0.02427d0*y**3     &
            &        +0.72085d0*y**4     &
            &        +0.01979d0*y**5     &
            &        -0.77530d0*y**6     &
            &        +0.32999d0*y**7
    else if (x >= 3.3d0 .and. x < 8.0d0) then
       if (x < 5.9d0) then
          Fa=    +0.0d0
       else
          Fa=    -0.044730d0*(x-5.9d0)**2 &
               & -0.009779d0*(x-5.9d0)**3
       end if
       cardelli1989A=+1.752d0                         &
            &        -0.316d0*  x                     &
            &        -0.104d0/((x-4.67d0)**2+0.341d0) &
            &        +Fa
    end if
    return
  end function cardelli1989A

  double precision function cardelli1989B(self,x)
    !% Return fitting function $a(x)$ for the dust attenuation model of \cite{cardelli_relationship_1989}.
    implicit none
    class           (stellarSpectraDustAttenuationCardelli1989), intent(inout) :: self
    double precision                                           , intent(in   ) :: x
    double precision                                                           :: y   , Fb

    cardelli1989B=0.0d0
    if      (x >= 0.3d0 .and. x < 1.1d0) then
       cardelli1989B=-0.52700d0*x**1.61d0
    else if (x >= 1.1d0 .and. x < 3.3d0) then
       y=x-1.82d0
       cardelli1989B=+1.41338d0*y        &
            &        +2.28305d0*y**2     &
            &        +1.07233d0*y**3     &
            &        -5.38434d0*y**4     &
            &        -0.62251d0*y**5     &
            &        +5.30260d0*y**6     &
            &        -2.09002d0*y**7
    else if (x >= 3.3d0 .and. x < 8.0d0) then
       if (x < 5.9d0) then
          Fb=    +0.0d0
       else
          Fb=    +0.2130d0*(x-5.9d0)**2 &
               & +0.1207d0*(x-5.9d0)**3
       end if
       cardelli1989B=-3.090d0                         &
            &        +1.825d0*  x                     &
            &        +1.206d0/((x-4.62d0)**2+0.263d0) &
            &        +Fb
    end if
    return
  end function cardelli1989B