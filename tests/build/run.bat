cd ..\..\build
cmd /c make -j8
cd ..\tests\build
cmd /c make coretranTest -B
cmd /c "..\bin\coretranTest 10 1"