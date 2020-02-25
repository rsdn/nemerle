@echo off
title %~nx0
rem                      Config  Target  Features
%~dp0Build-core-base.cmd Debug   Build   "Stage1;_PegAndCSharp;_ComputationExpressions;_Async;Install"