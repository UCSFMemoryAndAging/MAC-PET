# Overview

In this repository you will find the code distributed to the collaborators who agreed to supply the data for the centiloids meta-analysis in Blazhenets et al. 2025 [currently submitted for publication]. 

## Licensing 

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# Instructions on how to run

The analysis is implemented in R markdown, and the output will consist of an HTML document, summary table, and figures. 

You will need to prepare the table in the specific format following the specific convention for variable names. Please check the Demo.CSV file in the enclosed demo folder as well as some description at the beginning of the script (Reproducibility_of_Centiloids_UCSF-2024.Rmd) to update variable names. Not all variables are required (e.g., you do not need visual_read column in your CSV to run the script if you do not have visual reas information and so on). 

Required are: id, cl, age, tracer, male(sex); optional great to have: dx, mmse, cdr, apoe, visual_read.

Please update the cohort name and path to your CSV file in Reproducibility_of_Centiloids_UCSF-2024.Rmd and Knit to HTML (https://rmarkdown.rstudio.com/authoring_quick_tour.html) to produce the output. If there are any problems with knitting, you can Run All first and then Knit the document in the same session.

Please check the produced Reproducibility_of_Centiloids_UCSF-2024.nb.html document and files in the output folder. If there is any inconsistency in the data, the script might give you error messages explaining what is missing. 

## Packages and versions

Analyses were implemented in R version 4.4.0 (2024-04-24)

- 'ggplot2': version 4.0.0
- 'mixtools': version 2.0.0
- 'cluster': version 2.1.6
- 'readxl': version 1.4.3
- 'dplyr': version 1.1.4
- 'lubridate': version 1.9.3
- 'gridExtra': version 2.3
- 'grid': version 4.4.0
- 'data.table': version 1.15.4
- 'cutpointr': version 1.1.2
- 'cluster': version 2.1.6
- 'ggpubr': version 0.6.0
- 'kableExtra': version 1.4.0
- 'boot': version 1.3-30
- 'pROC': version 1.18.5
- 'car': version 3.1-2
- 'tidyr': version 1.3.1
- 'MASS': version 7.3-60.2

## Example output

The example of the produced output using the Demo dataset is included in the /output folder. Please note, the Demo dataset is a synthetic dataset created for demonstration purposes only and does not represent real amyloid PET data.

## Contact

Any comment or Inquiry is welcome. Please contact me at: ganna.blazhenets@ucsf.edu