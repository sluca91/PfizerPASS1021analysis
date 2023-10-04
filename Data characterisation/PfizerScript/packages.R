#Packages needed

if(!require(stringr)){install.packages("stringr")}
suppressPackageStartupMessages(library(stringr))

if(!require(data.table)){install.packages("data.table")}
suppressPackageStartupMessages(library(data.table))

if(!require(lubridate)){install.packages("lubridate")}
suppressPackageStartupMessages(library(lubridate))

if(!require("rlist")){install.packages("rlist")}
suppressPackageStartupMessages(library("rlist"))

if(!require("DBI")){install.packages("DBI")}
suppressPackageStartupMessages(library(DBI))

if(!require("Rcpp")){install.packages("Rcpp")}
suppressPackageStartupMessages(library("Rcpp"))

if(!require("sqldf")){install.packages("sqldf")}
suppressPackageStartupMessages(library("sqldf"))

if(!require("RSQLite")){install.packages("RSQLite")}
suppressPackageStartupMessages(library(RSQLite))

if(!require("parallel")){install.packages("parallel")}
suppressPackageStartupMessages(library(parallel))

if(!require("openxlsx")){install.packages("openxlsx")}
suppressPackageStartupMessages(library(openxlsx))

if(!require("english")){install.packages("english")}
suppressPackageStartupMessages(library("english"))

if(!require("peakRAM")){install.packages("peakRAM")}
suppressPackageStartupMessages(library("peakRAM"))

if(!require(glue)){install.packages("glue")}
library(glue)

if(!require("logr")){install.packages("logr")}
library(logr)

if(!require("zip")){install.packages("zip")}
library(zip)

if(!require(cobalt)){install.packages("cobalt")}
library(cobalt)

if(!require(WeightIt)){install.packages("WeightIt")}
library(WeightIt)



#if(!require("cgwtools")){install.packages("cgwtools")}
#if(require("cgwtools")) detach("package:cgwtools", unload=TRUE)

