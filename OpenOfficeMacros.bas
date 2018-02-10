REM  *****  BASIC  *****

Sub Main
	
End Sub

Sub WriteAsCsv

	Dim Propval(1) as New com.sun.star.beans.PropertyValue
	Propval(0).Name = "FilterName"
	Propval(0).Value = "Text - txt - csv (StarCalc)"
	Propval(1).Name = "FilterOptions"
	Propval(1).Value ="9,0,0,1,1"   'ASCII  59 = ;  34 = "
	Doc = ThisComponent
	FileName = "~/z80/dizzy/TestFile.txt"  'Change to whatever file name you want
	FileURL = convertToURL(FileName)
	Doc.StoreAsURL(FileURL, Propval())

End Sub