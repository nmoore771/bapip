import glob
stats = []
for filename in glob.glob("*.hs") :
	print("Statistics for file : " + filename)
	with open(filename, "r") as fh :
		s = fh.read()
		# LOC (With Comments)
		print("\tLOC (w Comments) : " + str(len(s.split("\n"))))
		# LOC (No Comments)
		i = 0
		sNC = ""
		dropping = False
		droppingTo = ''
		while i < len(s)-1 :
			if (dropping) :
				if (droppingTo == '\n') :
					if (s[i] == "\n" ) :
						dropping = False
						droppingTo = ""					
				elif (droppingTo == '-}') :
					if (s[i:i+2] == "-}" ) :
						dropping = False
						droppingTo = "" 
			else :
				if (s[i:i+2] == "--" ) :
					dropping = True
					droppingTo = "\n"
				elif (s[i:i+2] == "{-" ) :
					dropping = True
					droppingTo = "-}"
				else :
					sNC = sNC + s[i] 
			i += 1
			
		print("\tLOC (wo Comments) : " + str(len(sNC.split("\n"))))
		print("\tFunctions : " + str(s.count("::")))
		
		
