#!/bin/sh
	i=$1
	"C:/Program Files/JAGS/JAGS-4.2.0/x64/bin/jags-terminal.exe" < sim.$i/script.cmd > sim.$i/jagsoutput.txt 2>&1 &

	echo $! > sim.$i/jagspid.txt
	