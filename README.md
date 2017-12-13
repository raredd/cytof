
<style>
#content img {
  float: right;
  margin: 0px 0px 15px 20px;
}
</style>

<div id="content">
<img src="out/fdl.gif" />
</div>


**X-Shift** is a population-finding algorithm that processes large datasets using fast kNN estimation of cell event density; it automatically arranges populations by a marker-based classification system ([Samusik et al. (2016) Nature Methods, 13(6), 493–496.](https://www.nature.com/articles/nmeth.3863)).

**VorteX** is a graphical tool for cluster analysis of multiparametric datasets in biology, especially single-cell data.

Software to be used during workshop: Vortex package (X-Shift is included)

Computer operating system: PC, Mac, or Linux (Mac/PC used by Computational Lead during workshop)

Install:

  - [Java 1.8 64-bit](http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html)
  
  - [Vortex stand-alone](https://github.com/nolanlab/vortex/releases)

Run:

  - Launch `VorteX.jar`
  
  - If prompted, enter the physical RAM size of your computer in GB (e.g., 16)
  
  - Import data, custer, force-directed layout
  
  - Export cluster assignments and FDL coordinates
  
  - Use [R script](01-xshift.R) for [additional](out/pl1.pdf) [figures](out/pl2.pdf)
  
  - See [Rmd](out/xshift.Rmd) or [html](out/xshift.html) for documentation and more info

Useful:
  
  - [Getting started](https://github.com/nolanlab/vortex/wiki/Getting-Started)
  
  - Data source: [An Immune Atlas of Clear Cell Renal Cell Carcinoma](https://www.ncbi.nlm.nih.gov/pubmed/28475899)
  
  - [nice!](https://en.wikipedia.org/wiki/Nice_(Unix))
