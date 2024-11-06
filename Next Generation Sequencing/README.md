# Next Generation Sequencing (NGS)

## Course Objectives

The aim of this course is to equip graduate students with advanced knowledge in NGS data analysis through bioinformatics and computational biology, both from theoretical and practical perspectives. It is designed for students interested in developing careers in sequencing technologies to address biological challenges related to proteins, genes, genomes, and their interactions.

## Learning Outcomes and Skills Acquired

Throughout this course, I developed several technical skills, including:

1. **Unix Command Line and Scripting:**
Gained proficiency in Unix commands for navigating directories, creating and modifying files, and using tools like SED and AWK to automate data processing tasks without opening files directly.

2. **Sequencing Data Processing:**
Learned techniques for cleaning third-generation sequencing reads, applying quality filters, removing adapters, and handling fastq formats.

3. **Genome and Transcriptome Assembly:**
Mastered assembly techniques, including reference-based assembly, de novo, and hybrid methods, as well as the unique challenges associated with each method.

4. **RNA-Seq and Transcriptomic Analysis:**
Acquired skills in analyzing RNA-Seq data, including read mapping, differential expression, and identifying sequencing errors to improve data quality.

5. **Gene Prediction and Functional Annotation:**
Understood the basics of gene prediction in prokaryotes and eukaryotes using homology-based and ab initio methods, and performed functional annotation using databases and specialized software.

6. **Advanced Variant Detection:**
Applied Bayesian frameworks for variant detection to study the evolution of drug resistance, with hands-on practice in mapping and variant analysis.

The course covered a broad range of topics, from an introduction to sequencing technologies and quality control to advanced topics like chimera removal in assemblies, gene functional annotation, and gene prediction. These skills have enhanced my ability to analyze and interpret NGS data in complex biological contexts.

## Assignments

**Author**: Margarida Pinheiro 

**Student ID**: 201805012

**Program**: Master in Bioinformatics and Computational Biology

### [Assignment 1](https://github.com/margarida-cardeano-pinheiro/Portfolio/blob/64412d2de4f15c6f58544670c6da020c41f7c091/Next%20Generation%20Sequencing/Assignment%201.pdf)​:

Downloaded sequencing reads from the SRA Toolkit and prepared them as FASTQ files with split files for paired-end reads.
Performed quality checks using FastQC to assess read quality, examining base sequence quality, sequence content, and adapter content.
Used Trimmomatic for adapter and low-quality base removal, followed by re-running FastQC to confirm improved read quality.
Converted FASTQ to FASTA format using SED commands and processed data with AWK for various conditions based on quality.

### Assignment 2​:

Processed BAM files to assess quality control (QC), read pairing, and alignment status using SAMtools.
Applied SAM flags for filtering based on reverse strand alignment, unmapped reads, and duplicate removal.
Conducted sorting and duplicate marking, then counted reads to confirm duplicate removal.
Analyzed CIGAR strings to interpret alignment and base matching, mismatches, and indels.

### Assignment 3:
Assembled and annotated the transcriptome for S. pombe, merging expression data and filtering transcripts based on TPM.
Used IGV to visualize specific genes within the genome and identify corresponding proteins, such as heat shock protein and alpha-glucosidase.

### Assignment​ 4:

Conducted variant calling with SAM/BAM files, removing duplicates and filtering variants.
Performed variant quality filtering, calculated variant counts, and assessed allele frequency.
Completed a Principal Component Analysis (PCA) to visualize SNP and indel patterns, identifying relationships between species based on genetic similarity.
