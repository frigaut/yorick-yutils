#!/usr/bin/env python

def pyk(self,msg):
    # sends string command to yorick
    sys.stdout.write(msg)
    sys.stdout.flush()
 
# if this flag set, yorick is blocked waiting for tyk_resume
_tyk_blocked=0

def pyk_sync(self):
    _tyk_blocked=1
    sys.stdout.write('-s+y-n+c-+p-y+k-')
    sys.stdout.flush()

def pyk_resume(self):
    sys.stdout.write('pyk_resume'+msg)
    sys.stdout.flush()
    _tyk_blocked=0
    
