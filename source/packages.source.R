#------------------------------------------------------------------------------!
# Source code: Packages x ####
#------------------------------------------------------------------------------!

#-------------------------------------------------------------------------------!
# Check packages 
#-------------------------------------------------------------------------------!
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")}
library(BiocManager)
options(repos = BiocManager::repositories())
if (!requireNamespace("magick", quietly = TRUE)) {
  install.packages("magick")}
if (!requireNamespace("EBImage", quietly = TRUE)) {
  BiocManager::install("EBImage")}
if (!requireNamespace("rstudioapi", quietly = TRUE)) {
  install.packages("rstudioapi")}
if (!requireNamespace("autothresholdr", quietly = TRUE)) {
  install.packages("autothresholdr")}
if (!requireNamespace("scales", quietly = TRUE)) {
  install.packages("scales")}
if (!requireNamespace("imager", quietly = TRUE)) {
  install.packages("imager")}
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")}
if (!requireNamespace("shinyBS", quietly = TRUE)) {
  install.packages("shinyBS")}
if (!requireNamespace("shinyjs", quietly = TRUE)) {
  install.packages("shinyjs")}
if (!requireNamespace("magrittr", quietly = TRUE)) {
  install.packages("magrittr")}
if (!requireNamespace("rhandsontable", quietly = TRUE)) {
  install.packages("rhandsontable")}
if (!requireNamespace("purrr", quietly = TRUE)) {
  install.packages("purrr")}


#-------------------------------------------------------------------------------!
# Load packages 
#-------------------------------------------------------------------------------!

library(magick)
library(EBImage)
library(shiny)
library(shinyjs)
library(imager)
library(magrittr)
library(rhandsontable)
library(purrr)

# help(package="EBImage")
# help(package="imager")
# help(package="magick")