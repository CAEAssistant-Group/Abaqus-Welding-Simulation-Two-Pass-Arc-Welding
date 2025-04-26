C                                                 ######################################################################
C                                                 #################      CAE Assistant Company          ################
C                                                 ##############         www CAEassistant com              #############
C                                                 ###########   Copy right © 2021 by CAE Assistant Company    ##########
C                                                 ###################################################################### 
C                                                                         CAE Assisitant Services: 
C                                                 Toturial Packages,Consultancy,Articles,Q&A,Video Gallery,Online Course
C                                                 ######################################################################
C                                                                      Need help with your project? 
C                                                   You can get initial free consultation from (Support@CAEassistant com)
C                                                 ######################################################################  
C                                                                      CAE Assistant  © All Rights Reserved

      SUBROUTINE DFLUX(FLUX,SOL,KSTEP,KINC,TIME,NOEL,NPT,COORDS,JLTYP,
	1           TEMP,PRESS,SNAME)
C     
      INCLUDE 'ABA_PARAM.INC'
	  	PARAMETER( zero = 0., one = 1.)
C     
      DIMENSION COORDS(3),FLUX(2),TIME(2)
      CHARACTER*80 SNAME
C   Input_Data_Symbols
      REAl*8 V, Eta, Voltage1, Voltage2, Amper1, Amper2, Q1, Q2,  
     1 a1, b1, a2, b2, cf, cr, f_f, f_r, x1, y1, z1, 
     2 x2, y2, z2, Pi, Coef, 
     3 Expression1, Expression2, Expression3, Expression4, Qf, Qr
 
!######################################## Welding Parameters ###########################################!
!####################Calculation of the Input Power of the Welding Heat Source (Q) #####################! 
!################################## Q = η*V*I = Eta * Voltage * Amper ##################################!   
           
      V = 0.002                 ! Welding Velocity (in Directin 3)(2mm/s)
      Eta= ! Hidden Lines         
      
      Q1=  ! Hidden Lines

!############### Dimensions of Goldak's Double Ellipsoid Heat Source Model >>(Figure 10)################!
!## Where a, b, and cf, cr Are the Lengths of the Semi-Axes of the Double Ellipsoid Heat Source Model ##!	 
!#######################################################################################################! 
 
      a1 =  ! Hidden Lines

	  
!############## The Heat Fraction Parameters in the Front (f_f) and Rear Quadrants (f_r) ################! 

      f_f = 0.5 
      ! Hidden Lines
      
!########################################################################################################!      
!## The Coordinates of the Parts Points Should be Updated based on the Coordinate System Placed on the ##!
!# Welding Surface(in Each Pass). Also, These Coordinates Should be Updated based on the Welding Speed. #!
!########################################################################################################!
!########################### Updating the Coordinates of the Parts for Pass-1 ###########################!

        x1 = COORDS(1)
      ! Hidden Lines

!########################### Updating the Coordinates of the Parts for Pass-2 ###########################!	  
	  
        x2 = COORDS(1)
        ! Hidden Lines

!########################################################################################################! 
!######################### Calculation of Goldak's Double Ellipsoid Heat Source #########################!
!########################################################################################################! 

      FLUX(1) = 0   ! FLUX(1): Q : Magnitude of Flux Flowing into the Model at Each Point
      FLUX(2) =     ! Hidden Lines
	  
      JLTYP = 1     ! Determination of the flux type: Number 1 is used for a body flux

!###### Calculation of the Distribution Inside the Front Quadrant (Qf) and the Rear Quadrant (Qr) ######!
!############################################# for Pass-1 ##############################################!

			   Pi =  4 * atan(one) 
			   Coef =   ! Hidden Lines 
	  
        IF(KSTEP.ge.3 .and. KSTEP .le. 12) THEN
              
			 Expression1 = Coef * f_f *   ! Hidden Lines
            
               FLUX(1)=   ! Hidden Lines
               FLUX(2) = 0
        END IF

!############################################# for Pass-2 ##############################################!

        IF(KSTEP.ge.14 .and. KSTEP .le. 23) THEN  
	  
 			   Expression1 =   ! Hidden Lines
                  
                 FLUX(1)= Qf +   ! Hidden Lines
        END IF
      RETURN
      END