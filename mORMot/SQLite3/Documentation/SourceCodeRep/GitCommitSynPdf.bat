@echo off
@echo FossilRepository=%1
@echo GitRepository=%2
@echo GitExe=%3
@echo DescFile=%4

@echo.
@echo.
@echo SynPDF repository
@echo -----------------

@cd ..\SynPDF
%3 add .
%3 commit -a --file=%4
%3 push

