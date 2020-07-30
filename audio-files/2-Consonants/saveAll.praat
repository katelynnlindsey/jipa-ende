clearinfo

# Zielverzeichnis zum Speichern:
#Verzichnispfad, in dem die Dateien liegen
form SaveAllSelectedData
	comment Save all SELECTED objects as .wav to?
	sentence output 
	comment Hänge das an Dateinamen dran
	sentence filenameappend 
endform

# Anzahl der selektieren Soundoubjekte
n = numberOfSelected ("Sound")

for i to n
    sound'i' = selected ("Sound", i)   
endfor

#Mache das, was in der Schleife steht für alle Soundobjekte:
for i to n
    select sound'i'	
    name$ = selected$("Sound")
    
    #Infoline: kann auskommentiert werden
    printline 'output$'\'name$'.wav

    #Speichere als .wav	
    Write to WAV file... 'output$''name$''filenameappend$'.wav
endfor
