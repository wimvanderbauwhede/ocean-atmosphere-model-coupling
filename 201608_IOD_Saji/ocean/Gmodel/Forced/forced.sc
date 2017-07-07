#!/bin/csh
#
#             forced.sc   --- runs forced Gmodel                         

set SCRAT    = $HOME/201608_IOD_Saji/ocean
set Rootdir  = $SCRAT/Gmodel
set Modeldir = $Rootdir/Model
set Rundir   = $Rootdir/Forced
set Winddir  = $Rootdir/Wind    

set Windfile = $Winddir/pacano99.ext    
set Maskfile = $Winddir/fsumask
set logfile  = $Rundir/forced.log

echo " forced.sc started at " >>& $logfile   
date                          >>& $logfile   
echo "                      " >>& $logfile   

set yymmbgn = 196601
set yymmend = 199912

@ yyi = $yymmbgn / 100
@ mmi = $yymmbgn - 100 * $yyi
@ yyf = $yymmend / 100
@ mmf = $yymmend - 100 * $yyf
@ lrun = 12 *( $yyf - $yyi ) + $mmf - $mmi 
@ lrun = 30 * $lrun

( cd $Winddir && make )

cp $Windfile $Rundir/fsuano.ext
cp $Maskfile $Rundir/

set runid  = 'Forced_run'
set input  = $Rundir/control.in
set nconti = 0
set ncoup  = 0
echo $runid   > $input
echo $nconti  >> $input
echo $ncoup   >> $input
echo $lrun    >> $input
echo $yymmbgn >> $input

cd $Rundir
nice +10 $Modeldir/Shallow  >>& $logfile

echo "                    " >>& $logfile   
echo "                    " >>& $logfile   
time                        >>& $logfile
echo "                    " >>& $logfile   

echo " forced.sc  ended      " >>& $logfile   
date                           >>& $logfile   

