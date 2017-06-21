#define GMCF_VERBOSE
#define GMCF_INTERPOL_SPACE
module gmcfAPIocean
    use gmcfAPI
    use gmcfInterpolation

    implicit none

    integer :: ocean_id
    integer :: sync_done, has_packets, fifo_empty
    integer :: t_sync, t_sync_prev, t_sync_step, t_inter, n_ticks_at_sync
    integer :: sim_time,  current_sync_config
    logical :: is_sync_point

    ! All time-related values are multiples of GMCF_TIME_QUANTUM
    integer, parameter :: GMCF_TIME_STEP_OCEAN = 1 ! number of quanta
    integer, parameter :: GMCF_TIME_OFFSET_OCEAN = 0 ! number of quanta

    integer, parameter ::  ATMOSPHERE_SUB_NX=5 ! 100
    integer, parameter ::  ATMOSPHERE_SUB_NY=5 ! 100
    real(4), parameter :: ATMOSPHERE_DX=15,ATMOSPHERE_DY=15
    real(4), parameter :: ATMOSPHERE_X0=20,ATMOSPHERE_Y0=20

    integer, parameter :: OCEAN_KP=26, OCEAN_DK=1
#ifdef GMCF_INTERPOL_SPACE
    integer, parameter :: OCEAN_NX=7,OCEAN_NY=7
    real(4), parameter :: OCEAN_DX=8,OCEAN_DY=8
    real(4), parameter :: OCEAN_X0=26,OCEAN_Y0=26
#else
!    integer, parameter :: ATMOSPHERE_IME=26,ATMOSPHERE_JME=28,ATMOSPHERE_KME=27
!    integer, parameter :: OCEAN_NX=48,OCEAN_NY=48,OCEAN_KP=27
!    integer, parameter :: OCEAN_NX=100,OCEAN_NY=100,OCEAN_KP=27

! If we don't want to interpolate then the OCEAN_* must be the same as the ATMO*
    integer, parameter :: OCEAN_NX=ATMOSPHERE_SUB_NX,OCEAN_NY=ATMOSPHERE_SUB_NY
    real(4), parameter :: OCEAN_DX=ATMOSPHERE_DX,OCEAN_DY=ATMOSPHERE_DY
    real(4), parameter :: OCEAN_X0=ATMOSPHERE_X0,OCEAN_Y0=ATMOSPHERE_Y0

#endif

    type(gmcfPacket) :: packet
    real(4), dimension(1:4,0:ATMOSPHERE_SUB_NX-1,0:ATMOSPHERE_SUB_NY-1) :: wind_profile, wind_profile_prev, wind_profile_current
    real(4), dimension(0:ATMOSPHERE_SUB_NX-1,0:ATMOSPHERE_SUB_NY-1) :: temperature
    save

contains
! ----------------------------------------------------------------------------------------------------------------
!$GMCF Init(timesteps=20) !if >1 it means we need to interpolate otherwise not
    subroutine gmcfInitOcean(sys,tile,m_id)
        integer(8), intent(In) :: sys
        integer(8), intent(In) :: tile
        integer, intent(In) :: m_id
#ifdef GMCF_VERBOSE
        print *, "OCEAN API gmcfInitOcean() BEFORE gmcfInitCoupler"
#endif
        ocean_id=m_id
!        t_ocean = 0
!        t_sync_prev = -1
!        t_sync = t_ocean
!        t_sync_step = 24
!        temperature=0.0
        call gmcfInitCoupler(sys,tile, ocean_id)
    end subroutine gmcfInitOcean
! ----------------------------------------------------------------------------------------------------------------
! n_ticks = (t-t_start)/t_step
    subroutine gmcfSyncOcean(n_ticks)
        integer, intent(In) :: n_ticks
!        t_sync = t_ocean / t_sync_step ! means ocean steps are smaller and more frequent than atmosphere
!        t_inter = mod(t_ocean,t_sync_step) ! for interpolation between two steps of atmosphere
!        t_ocean = t_ocean + 1

        sim_time = n_ticks * GMCF_TIME_STEP_OCEAN + GMCF_TIME_OFFSET_OCEAN
#ifdef GMCF_VERBOSE
        print *, "OCEAN API gmcfSyncOcean",sim_time
#endif
        call gmcfCheckSync(ocean_id,sim_time,current_sync_config, is_sync_point)

        if (is_sync_point) then
            n_ticks_at_sync = n_ticks
            if (gmcfStatus(GMCF_ATMOSPHERE_ID) /= FIN) then
        ! Sync will synchronise simulation time steps but also handle any pending requests
        !$GMC sync(t)

#ifdef GMCF_VERBOSE
                print *, "OCEAN API BEFORE gmcfSync()",sim_time,sync_done
#endif
                    sync_done=0
                    do while(sync_done == 0)
                        call gmcfSync(ocean_id,sim_time,sync_done)
#ifdef GMCF_VERBOSE
                        print *, "OCEAN API AFTER gmcfSync()"
                        print *, "OCEAN API", ocean_id," sync loop ",sim_time,"..."
#endif
                    end do
#ifdef GMCF_VERBOSE
                print *, "OCEAN API", ocean_id," syncing DONE for time step ",sim_time, ", ", n_ticks, " ticks"
#endif
            end if ! FIN
        end if ! is_sync_point
    end subroutine gmcfSyncOcean
! ----------------------------------------------------------------------------------------------------------------
    subroutine gmcfPreOcean(u,v,w, t_surface)
        real(kind=4), dimension(OCEAN_NX,OCEAN_NY,OCEAN_KP), intent(InOut) :: u,v,w
        real(kind=4), dimension(OCEAN_NX,OCEAN_NY), intent(In) :: t_surface

        if (is_sync_point) then
            ! So now we can do some work. Let's suppose ocean is the OCEAN, and it requests data from model2, ATMOSPHERE.
            ! First overwrite the *prev vars with the current vars
#if GMCF_INTERPOL_TIME
        wind_profile_prev = wind_profile
#endif
        if (gmcfStatus(GMCF_OCEAN_ID) /= FIN) then
                select case (current_sync_config) ! <code for the variable var_name, GMCF-style>
                    case (OCEAN_ATMOSPHERE_TEMPERATURE)

                        call gmcfSampleTemperatureOcean(t_surface)
                        print *, "OCEAN API: gmcfSampleTemperatureOcean(t_surface) BB", t_surface(1,1), temperature(0,0)
                        print *, "OCEAN API: gmcfSampleTemperatureOcean(t_surface) EE", t_surface(OCEAN_NX,OCEAN_NY), temperature(ATMOSPHERE_SUB_NX-1,ATMOSPHERE_SUB_NY-1), sum(temperature), sum(t_surface)
                        print *, "OCEAN API: gmcfSend2DFloatArray(temperature)"
                        call gmcfSend2DFloatArray(ocean_id,temperature, shape(temperature), GMCF_TEMPERATURE, GMCF_ATMOSPHERE_ID,PRE,sim_time)
!                    case (ATMOSPHERE_OCEAN_WIND_PROFILE)
                        call gmcfWaitFor(ocean_id,RESPDATA, GMCF_ATMOSPHERE_ID, 1)
#ifdef GMCF_VERBOSE
                        print *, "OCEAN API: got 1 DRESP ..."
#endif
                        ! and then we read them
                        call gmcfHasPackets(ocean_id,RESPDATA,has_packets)
                        do while (has_packets==1)
                            call gmcfShiftPending(ocean_id,GMCF_ATMOSPHERE_ID,RESPDATA,packet,fifo_empty)
                            ! read a packet
                            select case (packet%data_id) ! <code for the variable var_name, GMCF-style>
                                case (GMCF_WIND_PROFILE)
                                    call gmcfRead3DFloatArray(wind_profile,shape(wind_profile), packet)
                                    print *, "OCEAN API: gmcfRead3DFloatArray(wind_profile)",wind_profile(1,0,0)
                                    ! so here we must assign the wind profile to u,v,w
                                    call gmcfInterpolateWindprofileSpaceOcean(u,v,w)
                                    print *, "OCEAN API: gmcfInterpolateWindprofileSpaceOcean(u,v,w) BB",wind_profile(1,0,0),u(1,1,1)
                                    print *, "OCEAN API: gmcfInterpolateWindprofileSpaceOcean(u,v,w) EE",wind_profile(1,ATMOSPHERE_SUB_NX-1,ATMOSPHERE_SUB_NY-1),u(OCEAN_NX,OCEAN_NY,1)
                            end select
                            call gmcfHasPackets(ocean_id,RESPDATA,has_packets)
                        end do
                        print *, "OCEAN API: SANITY: ",sum(wind_profile)
#ifdef GMCF_VERBOSE
                        print *, "OCEAN API: DONE reading DRESP into vars, ready to compute ..."
#endif
                    case default
                        print *, "OCEAN API: current_sync_config = ",current_sync_config," => NO ACTION"
                    ! probably need a default "do nothing" here
                end select
        end if ! FIN
        end if ! is_sync_point

    end subroutine gmcfPreOcean
! ----------------------------------------------------------------------------------------------------------------
    subroutine gmcfPostOcean
!        real(kind=4), dimension(128,128), intent(In) :: wind_profile

        if (gmcfStatus(GMCF_ATMOSPHERE_ID) /= FIN) then
        ! Wait for one post data request
#ifdef GMCF_VERBOSE
            print *,"OCEAN API: POST SHOULD BE OBSOLETE"
#endif


        end if ! FIN
    end subroutine gmcfPostOcean
! ----------------------------------------------------------------------------------------------------------------
    ! The wind_profile from the atmosphere model must be interpolated in space as well as in time
    ! In this case it is best to do the time interpolation first, as that is on smaller arrays
    ! But that is not always the case
    subroutine gmcfInterpolateWindprofileTimeOcean(n_ticks)
        integer, intent(In) :: n_ticks
        integer :: k,jj,ii
        do jj=0,ATMOSPHERE_SUB_NY-1
            do ii=0,ATMOSPHERE_SUB_NX-1
                do k = 1,3
                    wind_profile_current(k,jj,ii) = gmcfInterpolateTime( wind_profile(k,jj,ii), wind_profile_prev(k,jj,ii), OCEAN_SYNC_PERIOD, n_ticks - n_ticks_at_sync)
                end do
            end do
        end do
    end subroutine gmcfInterpolateWindprofileTimeOcean
! ----------------------------------------------------------------------------------------------------------------
    ! Once we have the time-interpolated values we must interpolate over space, for that I should probably use the third-party routines
    subroutine gmcfInterpolateWindprofileSpaceOcean(u,v,w)
! The values of OCEAN_OCEAN_NX,OCEAN_OCEAN_NY,OCEAN_OCEAN_KP must be known at code generation time!
        real(kind=4), dimension(OCEAN_NX,OCEAN_NY,OCEAN_KP), intent(InOut)  :: u
        real(kind=4), dimension(OCEAN_NX,OCEAN_NY,OCEAN_KP), intent(InOut)  :: v
        real(kind=4), dimension(OCEAN_NX,OCEAN_NY,OCEAN_KP), intent(InOut)  :: w
#ifdef GMCF_INTERPOL_SPACE
        real(kind=4), dimension(6) :: grid_wp = (/ real(ATMOSPHERE_SUB_NX), ATMOSPHERE_DX, ATMOSPHERE_X0,real(ATMOSPHERE_SUB_NY),ATMOSPHERE_DY,ATMOSPHERE_Y0 /)
        real(kind=4), dimension(6) :: grid_uvw = (/ real(OCEAN_NX),OCEAN_DX,OCEAN_X0,real(OCEAN_NY), OCEAN_DY,OCEAN_Y0 /)

        call gmcf2DInterpolationConstSpacing(wind_profile(1,:,:),grid_wp,u(:,:,1),grid_uvw)
        call gmcf2DInterpolationConstSpacing(wind_profile(2,:,:),grid_wp,v(:,:,1),grid_uvw)
        call gmcf2DInterpolationConstSpacing(wind_profile(3,:,:),grid_wp,w(:,:,1),grid_uvw)
!
!        call gmcf2DInterpolation(wind_profile(1,:,:),shape( wind_profile(1,:,:) ) ,grid_wp,u(:,:,1),shape( u(:,:,1) ),grid_uvw)
!        call gmcf2DInterpolation(wind_profile(2,:,:),shape( wind_profile(2,:,:) ) ,grid_wp,v(:,:,1),shape(v(:,:,1)),grid_uvw)
!        call gmcf2DInterpolation(wind_profile(3,:,:),shape( wind_profile(3,:,:) ) ,grid_wp,w(:,:,1),shape(w(:,:,1)),grid_uvw)
#else
        ! TEST 1
!        u(1,1,1) = wind_profile(1,0,0)
!        v(1,1,1) = wind_profile(2,0,0)
!        w(1,1,1) = wind_profile(3,0,0)
!        ! TEST 2
        u(:,:,1) = wind_profile(1,:,:)
        v(:,:,1) = wind_profile(2,:,:)
        w(:,:,1) = wind_profile(3,:,:)
#endif
    end subroutine gmcfInterpolateWindprofileSpaceOcean
! ----------------------------------------------------------------------------------------------------------------
    ! The temperature array to be sent to the atmosphere model is sampled from the original temperature array in the ocean model
    subroutine gmcfSampleTemperatureOcean(t_ocean)
        real(kind=4), dimension(OCEAN_NX,OCEAN_NY), intent(In)  :: t_ocean
        ! In general, sampling requires interpolation
#ifdef GMCF_INTERPOL_SPACE
        integer :: i,j
        real(kind=4), dimension(6) :: grid_ocean = (/ real(OCEAN_NX),OCEAN_DX,OCEAN_X0, real(OCEAN_NY), OCEAN_DY,OCEAN_Y0 /)
        real(kind=4), dimension(6) :: grid_temp = (/ real(ATMOSPHERE_SUB_NX), ATMOSPHERE_DX, ATMOSPHERE_X0, real(ATMOSPHERE_SUB_NY),ATMOSPHERE_DY,ATMOSPHERE_Y0 /)

        do i=1,OCEAN_NX
!            do j=1,OCEAN_NY
!                print *, "gmcfSampleTemperatureOcean ORIG:", i,j,t_ocean(i,j)
                print "('gmcfSampleTemperatureOcean ORIG:'10f12.2)", ( t_ocean(i,j), j=1,OCEAN_NY )
!            end do
        end do

        do i=0,ATMOSPHERE_SUB_NX-1
!                print *, "gmcfSampleTemperatureOcean PREV:", i,j,temperature(i,j)
                print "('gmcfSampleTemperatureOcean PREV:'10f12.2)", ( temperature(i,j), j=0,ATMOSPHERE_SUB_NY-1 )
        end do

        call gmcf2DInterpolationConstSpacing(t_ocean,grid_ocean,temperature,grid_temp)

        do i=0,ATMOSPHERE_SUB_NX-1
!            do j=0,ATMOSPHERE_SUB_NY-1
!                print *, "gmcfSampleTemperatureOcean SAMPLED:", i,j,temperature(i,j)
                print "('gmcfSampleTemperatureOcean SAMPLED:'10f12.2)", ( temperature(i,j), j=0,ATMOSPHERE_SUB_NY-1 )
!            end do
        end do
#else
        ! TEST 1

!        temperature(0,0)=t_ocean(1,1)
!        temperature(0:,0:) = t_ocean(1:,1:)
        temperature = t_ocean
        print *, "gmcfSampleTemperatureOcean:", t_ocean(1,1), '=',temperature(0,0),';',t_ocean(OCEAN_NX,OCEAN_NY),temperature(ATMOSPHERE_SUB_NX-1,ATMOSPHERE_SUB_NY-1)
#endif
    end subroutine gmcfSampleTemperatureOcean
! ----------------------------------------------------------------------------------------------------------------
    subroutine gmcfFinishedOcean
        call gmcfFinished(ocean_id)
    end subroutine gmcfFinishedOcean

end module gmcfAPIocean
