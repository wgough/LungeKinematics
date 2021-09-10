#' Lunge Kinematics and Energetics Dataset For Gough et al. 2021
#'
#' A dataset containing kinematics and stuff...
#'
#' @format A data frame with 1260 rows and 56 (for now) variables:
#' \describe{
#'   \item{LungeI}{Data index of a lunge in the deployment's PRH file}
#'   \item{timeoflunge}{time of a lunge, local}
#' }
#' @source \url{http://www.FIXME.info/}
"AllWhalesLungeTableTrunc"

#' Lunge Kinematics and Energetics Whale Average Values
#'
#' Values from AllWhalesLungeTableTrunc have been consolidated into mean values.
#'
#' @format A data frame with 42 rows and 13 (for now) variables:
#' \describe{
#'   \item{Species}{The species of that particular whale}
#'   \item{whaleName}{That particular whale's ID}
#' }
#' @source \url{http://www.FIXME.info/}
"AllWhalesAvgs"

#' Lunge Kinematics and Energetics Whale Average Values + Filtration Data
#'
#' Values from AllWhalesLungeTableTrunc have been consolidated into mean values and filtration data has been added from Shirel. Lunges without filtration data have been filtered out of the dateset.
#'
#' @format A data frame with 28 rows and 10 (for now) variables:
#' \describe{
#'   \item{Species}{The species of that particular whale}
#'   \item{whaleName}{That particular whale's ID}
#' }
#' @source \url{http://www.FIXME.info/}
"AllWhalesAvgsWithFilt"

#' Lunge Kinematics and Energetics Whale Average Values + Filtration Data (CHANGE)
#'
#' Values from AllWhalesLungeTableTrunc have been consolidated into mean values and filtration data has been added from Shirel. Lunges without filtration data have been filtered out of the dateset.
#'
#' @format A data frame with 28 rows and 10 (for now) variables:
#' \describe{
#'   \item{Species}{The species of that particular whale}
#'   \item{whaleName}{That particular whale's ID}
#' }
#' @source \url{http://www.FIXME.info/}
"LitMaxSpeeds"
