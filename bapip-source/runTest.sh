SOURCE=testingArena/TrafficSignals.bsv
MODULE=TrafficSignals
DEST=testingArena/TrafficSignals

#SOURCE=testingArena/SchedTest/BSV/SchedTest.bsv
#MODULE=ScedTest
#DEST=testingArena/SchedTest/PVS

stack build
stack exec BAPIP-exe bsv2pvs $SOURCE $MODULE $DEST 2> log.log

