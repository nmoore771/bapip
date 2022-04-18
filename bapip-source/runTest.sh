# A -> Traffic Signal example, S 5.2.1
# B -> Limits Alarm case study, S 8.1
# C -> Alrm_int case study, S 8.2
# D -> RapidIO Packet Decoder case study, S 8.3
# E -> RapidIO Packet Encoder case study, S 8.4
# F -> RapidIO Transaction ID case study, S 8.5

if [ $# -eq 0 ] 
then
	SELECT=A
else 
	SELECT="$1"
fi


if [ "$SELECT" == "A" ] 
then 
	SRC=testingArena/Input/TrafficSignals/TrafficSignals.bsv
	MOD=TrafficSignals
	DST=testingArena/Output/TrafficSignals
elif [ "$SELECT" == "B" ] 
then 
	SRC=testingArena/Input/LimitsAlarm/LIMITS_ALARM.bsv
	MOD=LIMITS_ALARM
	DST=testingArena/Output/LIMITS_ALARM
elif [ "$SELECT" == "C" ] 
then 
	SRC=testingArena/Input/AlrmInt/ALRM_INT.tsp
	MOD=
	DST=testingArena/Output/ALRM_INT
elif [ "$SELECT" == "D" ] 
then 
	SRC=testingArena/Input/RapidIODecoder/RapidIO_TgtDecoder_ByteCnt_ByteEn.bsv
	MOD=
	DST=testingArena/Output/DECODER
elif [ "$SELECT" == "E" ] 
then 
	SRC=testingArena/Input/RapidIOEncoder/RapidIO_InitEncoder_WdPtr_Size.bsv
	MOD=RapidIO_InitEncoder_WdPtr_Size
	DST=testingArena/Output/ENCODER
elif [ "$SELECT" == "F" ] 
then 
	SRC=testingArena/Input/RapidIOTID/RapidIO_MainCore.bsv
	MOD=Ifc_RapidIO_MainCore
	DST=testingArena/Output/TID
fi

stack build


if [ "$SELECT" == "C" ] 
then
	time stack exec BAPIP-exe tsp2pvs $SRC $DST 2> errors.log
else
	time stack exec BAPIP-exe bsv2pvs $SRC $MOD $DST 2> errors.log
fi
