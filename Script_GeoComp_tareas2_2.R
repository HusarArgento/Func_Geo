install.packages("rgee")
library(rgee)
ee_Initialize()
#Respuesta:
#Downloading cpython-3.12.14-windows-x86_64-none (download) (21.0MiB)
#Downloaded cpython-3.12.14-windows-x86_64-none (download)
#Downloading numpy (12.0MiB)
#Downloaded numpy
#Installed 1 package in 3.75s
#Error en ee_connect_to_py(path = ee_current_version, n = 5): 
#  The current Python PATH: C:/Users/lucas/AppData/Local/R/cache/R/reticulate/uv/cache/archive-v0/PQw772RzgO0RD9OL/Scripts/python.exe
#does not have the Python package "earthengine-api" installed. Do you restarted/terminated
#your R session after install miniconda or run ee_install()?
#  If this is not the case, try:
#  > ee_install_upgrade(): Install the latest Earth Engine Python version.
#> reticulate::use_python(): Refresh your R session and manually set the Python environment with all rgee dependencies.
#> ee_install(): To create and set a Python environment with all rgee dependencies.
#> ee_install_set_pyenv(): To set a specific Python environment.
#Modify Chunk OptionsRun All Chunks AboveRun Current Chunk

cuencas <- ee$FeatureCollection(
  "WWF/HydroSHEDS/v1/Basins/hybas_12")

#Respuesta:
#Error: Python module ee was not found.
#Detected Python configuration:

#python:         C:/Users/lucas/AppData/Local/R/cache/R/reticulate/uv/cache/archive-v0/PQw772RzgO0RD9OL/Scripts/python.exe
#libpython:      C:/Users/lucas/AppData/Local/R/cache/R/reticulate/uv/python/cpython-3.12.14-windows-x86_64-none/python312.dll
#pythonhome:     C:/Users/lucas/AppData/Local/R/cache/R/reticulate/uv/cache/archive-v0/PQw772RzgO0RD9OL
#virtualenv:     C:/Users/lucas/AppData/Local/R/cache/R/reticulate/uv/cache/archive-v0/PQw772RzgO0RD9OL/Scripts/activate_this.py
#version:        3.12.14 (main, Sep  1 2026, 14:17:39) [MSC v.1944 64 bit (AMD64)]
#Architecture:   64bit
#numpy:          C:/Users/lucas/AppData/Local/R/cache/R/reticulate/uv/cache/archive-v0/PQw772RzgO0RD9OL/Lib/site-packages/numpy
#numpy_version:  2.5.3
#ee:             [NOT FOUND]
#NOTE: Python version was forced by py_require()

ee_clean_pyenv()
ee_install()

#────────── Python configuration used to create rgee ──
#python:         C:/Users/lucas/AppData/Local/R/cache/R/reticulate/uv/cache/archive-v0/PQw772RzgO0RD9OL/Scripts/python.exe
#libpython:      C:/Users/lucas/AppData/Local/R/cache/R/reticulate/uv/python/cpython-3.12.14-windows-x86_64-none/python312.dll
#pythonhome:     C:/Users/lucas/AppData/Local/R/cache/R/reticulate/uv/cache/archive-v0/PQw772RzgO0RD9OL
#virtualenv:     C:/Users/lucas/AppData/Local/R/cache/R/reticulate/uv/cache/archive-v0/PQw772RzgO0RD9OL/Scripts/activate_this.py
#version:        3.12.14 (main, Sep  1 2026, 14:17:39) [MSC v.1944 64 bit (AMD64)]
#Architecture:   64bit
#numpy:          C:/Users/lucas/AppData/Local/R/cache/R/reticulate/uv/cache/archive-v0/PQw772RzgO0RD9OL/Lib/site-packages/numpy
#numpy_version:  2.5.3
#ee:             [NOT FOUND]
#NOTE: Python version was forced by py_require()
#─────────────────────────────────────────────────────── 
#Error en value[[3L]](cond): 
#  An error occur when ee_install was creating the Python Environment. Run ee_clean_pyenv() and restart the R session, before trying again.

library(reticulate)
ee_install()

#─────────────────────────────────────────── Python configuration used to create rgee ──
#python:         C:/Users/lucas/AppData/Local/R/cache/R/reticulate/uv/cache/archive-v0/PQw772RzgO0RD9OL/Scripts/python.exe
#libpython:      C:/Users/lucas/AppData/Local/R/cache/R/reticulate/uv/python/cpython-3.12.14-windows-x86_64-none/python312.dll
#pythonhome:     C:/Users/lucas/AppData/Local/R/cache/R/reticulate/uv/cache/archive-v0/PQw772RzgO0RD9OL
#virtualenv:     C:/Users/lucas/AppData/Local/R/cache/R/reticulate/uv/cache/archive-v0/PQw772RzgO0RD9OL/Scripts/activate_this.py
#version:        3.12.14 (main, Sep  1 2026, 14:17:39) [MSC v.1944 64 bit (AMD64)]
#Architecture:   64bit
#numpy:          C:/Users/lucas/AppData/Local/R/cache/R/reticulate/uv/cache/archive-v0/PQw772RzgO0RD9OL/Lib/site-packages/numpy
#numpy_version:  2.5.3
#ee:             [NOT FOUND]

#NOTE: Python version was forced by py_require()
#─────────────────────────────────────────────────────────────────────────────────────── 

#EnvironmentLocationNotFound: Not a conda environment: C:\Users\lucas\AppData\Local\R-MINI~1\envs\rgee

#Channels:
  #  - conda-forge
#Platform: win-64
#Collecting package metadata (repodata.json): ...working... done
#Solving environment: ...working... done

## Package Plan ##

#environment location: C:\Users\lucas\AppData\Local\R-MINI~1\envs\rgee

#added / updated specs:
#  - python=3.13


#The following packages will be downloaded:
  
#  package                    |            build
#---------------------------|-----------------
#  libpython-3.13.15          |h4f90d01_103_cp313          48 KB  conda-forge
#python-3.13.15             |h254dcb4_103_cp313        15.7 MB  conda-forge
#python_abi-3.13            |          9_cp313           7 KB  conda-forge
#------------------------------------------------------------
#  Total:        15.8 MB

#The following NEW packages will be INSTALLED:
  
  #  bzip2              conda-forge/win-64::bzip2-1.0.8-h0ad9c76_10 
  #ca-certificates    conda-forge/noarch::ca-certificates-2026.7.22-h4c7d964_0 
  #libexpat           conda-forge/win-64::libexpat-2.8.1-hac47afa_1 
  #libffi             conda-forge/win-64::libffi-3.7.0-h3d046cb_1 
  #liblzma            conda-forge/win-64::liblzma-5.8.3-hfd05255_1 
  #libmpdec           conda-forge/win-64::libmpdec-4.0.0-hfd05255_2 
  #libpython          conda-forge/win-64::libpython-3.13.15-h4f90d01_103_cp313 
  #libsqlite          conda-forge/win-64::libsqlite-3.53.4-hf5d6505_1 
  #libzlib            conda-forge/win-64::libzlib-1.3.2-hfd05255_3 
  #openssl            conda-forge/win-64::openssl-3.6.4-hf411b9b_0 
  #pip                conda-forge/noarch::pip-26.2.1-pyh145f28c_0 
  #python             conda-forge/win-64::python-3.13.15-h254dcb4_103_cp313 
  #python_abi         conda-forge/noarch::python_abi-3.13-9_cp313 
  #tk                 conda-forge/win-64::tk-8.6.13-h967ab96_4 
  #tzdata             conda-forge/noarch::tzdata-2026c-h151e31d_0 
  #ucrt               conda-forge/win-64::ucrt-10.0.26100.0-h57928b3_0 
  #vc                 conda-forge/win-64::vc-14.5-ha367084_41 
  #vc14_runtime       conda-forge/win-64::vc14_runtime-14.51.36247-habf1de7_41 
  #vcomp14            conda-forge/win-64::vcomp14-14.51.36247-habf1de7_41 


#Preparing transaction: ...working... done
#Verifying transaction: ...working... done
#Executing transaction: ...working... done
#WARNING conda.conda_pypi.main:notify_externally_managed_future(156): 
#  Did you know? You can install many PyPI packages with conda
#using the conda-pypi beta. Get started:
#  https://docs.conda.io/projects/conda/en/stable/new-features.html

#Channels:
#  - conda-forge
#Platform: win-64
#Collecting package metadata (repodata.json): ...working... done
#Solving environment: ...working... done

## Package Plan ##

#environment location: C:\Users\lucas\AppData\Local\r-miniconda\envs\rgee

#added / updated specs:
  #  - earthengine-api
#- ee_extra
#- numpy


#The following packages will be downloaded:
  
  #  package                    |            build
#---------------------------|-----------------
#aiohappyeyeballs-2.7.1     |     pyhd8ed1ab_0          24 KB  conda-forge
#aiohttp-3.14.3             |  py313h51e1470_0        1018 KB  conda-forge
#aiosignal-1.4.0            |     pyhd8ed1ab_0          13 KB  conda-forge
#attrs-26.1.0               |     pyhcf101f3_0          63 KB  conda-forge
#backports.zstd-1.7.0       |  py313h2a31948_1         237 KB  conda-forge
#brotli-python-1.2.0        |  py313h3c654a2_4         329 KB  conda-forge
#c-ares-1.34.8              |       h6a83c73_2         206 KB  conda-forge
#cffi-2.1.1                 |  py313h5ea7bf4_3         338 KB  conda-forge
#cpython-3.13.15            |py313hd8ed1ab_103          48 KB  conda-forge
#cryptography-50.0.1        |  py311h67a9792_1         1.7 MB  conda-forge
#earthengine-api-1.7.43     |     pyhd8ed1ab_1         323 KB  conda-forge
#ee_extra-2025.7.2          |     pyhd8ed1ab_0         200 KB  conda-forge
#frozenlist-1.8.0           |  py313h0c48a3b_0          49 KB  conda-forge
#google-api-core-2.36.0     |     pyh5ded981_0         110 KB  conda-forge
#google-api-core-grpc-2.36.0|     pyh24e1d2c_0          11 KB  conda-forge
#google-api-python-client-2.200.0|     pyh5ded981_1         6.7 MB  conda-forge
#google-auth-2.58.0         |     pyh5ded981_0         155 KB  conda-forge
#google-auth-httplib2-0.4.2 |     pyhcf101f3_0          20 KB  conda-forge
#google-cloud-core-2.7.0    |     pyhcf101f3_0          33 KB  conda-forge
#google-cloud-storage-3.14.1|     pyh5ded981_0         210 KB  conda-forge
#google-crc32c-1.8.0        |  py313h5327936_2          32 KB  conda-forge
#google-resumable-media-2.10.2|     pyhcf101f3_0          48 KB  conda-forge
#googleapis-common-protos-1.75.3|     pyh5ded981_0         147 KB  conda-forge
#googleapis-common-protos-grpc-1.75.3|     pyh5ded981_0          12 KB  conda-forge
#grpc-google-iam-v1-0.14.5  |     pyhcf101f3_0          31 KB  conda-forge
#grpcio-1.83.1              |  py313hc28e21e_1         864 KB  conda-forge
#grpcio-status-1.83.1       |     pyhcf101f3_0          28 KB  conda-forge
#httplib2-0.32.0            |     pyhcf101f3_0          92 KB  conda-forge
#importlib-metadata-8.7.1   |     pyhcf101f3_0          34 KB  conda-forge
#libabseil-20260526.0       | cxx17_h04e5de1_2         1.9 MB  conda-forge
#libcrc32c-1.1.2            |       h0e60522_0          25 KB  conda-forge
#libgrpc-1.83.1             |       he6cc664_1        11.2 MB  conda-forge
#libprotobuf-7.35.1         |       h1a71995_3         7.2 MB  conda-forge
#libre2-11-2025.11.05       |       h45f70d9_2         260 KB  conda-forge
#multidict-6.7.1            |  py313hd650c13_0          90 KB  conda-forge
#numpy-2.5.3                |  py313ha8dc839_0         7.1 MB  conda-forge
#opentelemetry-api-1.44.0   |     pyhd8ed1ab_0          51 KB  conda-forge
#propcache-0.5.2            |  py313hd650c13_0          47 KB  conda-forge
#proto-plus-1.28.4          |     pyhcf101f3_0          43 KB  conda-forge
#protobuf-7.35.1            |  py313h91c73f5_3         493 KB  conda-forge
#pyasn1-0.6.4               |     pyhcf101f3_0          65 KB  conda-forge
#pyasn1-modules-0.4.2       |     pyhd8ed1ab_0          94 KB  conda-forge
#pycparser-3.0              |     pyhcf101f3_0          55 KB  conda-forge
#pyparsing-3.3.2            |     pyhcf101f3_0         108 KB  conda-forge
#python-gil-3.13.15         |     h4df99d1_103          48 KB  conda-forge
#pyu2f-0.1.5                |     pyhd8ed1ab_1          36 KB  conda-forge
#re2-2025.11.05             |       haef9524_2         217 KB  conda-forge
#rsa-4.9.1                  |     pyhd8ed1ab_0          31 KB  conda-forge
#six-1.17.0                 |     pyhe01879c_1          18 KB  conda-forge
#uritemplate-4.2.0          |     pyhd8ed1ab_0          16 KB  conda-forge
#vs2015_runtime-14.51.36247 |      h633cb9f_41          21 KB  conda-forge
#yarl-1.24.5                |  py313hd650c13_0         164 KB  conda-forge
#------------------------------------------------------------
#  Total:        42.1 MB

#The following NEW packages will be INSTALLED:
  
#  _python_abi3_supp~ conda-forge/noarch::_python_abi3_support-1.0-hd8ed1ab_3 
#aiohappyeyeballs   conda-forge/noarch::aiohappyeyeballs-2.7.1-pyhd8ed1ab_0 
#aiohttp            conda-forge/win-64::aiohttp-3.14.3-py313h51e1470_0 
#aiosignal          conda-forge/noarch::aiosignal-1.4.0-pyhd8ed1ab_0 
#attrs              conda-forge/noarch::attrs-26.1.0-pyhcf101f3_0 
#backports.zstd     conda-forge/win-64::backports.zstd-1.7.0-py313h2a31948_1 
#brotli-python      conda-forge/win-64::brotli-python-1.2.0-py313h3c654a2_4 
#c-ares             conda-forge/win-64::c-ares-1.34.8-h6a83c73_2 
#certifi            conda-forge/noarch::certifi-2026.7.22-pyhd8ed1ab_0 
#cffi               conda-forge/win-64::cffi-2.1.1-py313h5ea7bf4_3 
#charset-normalizer conda-forge/noarch::charset-normalizer-3.5.1-pyhd8ed1ab_0 
#cpython            conda-forge/noarch::cpython-3.13.15-py313hd8ed1ab_103 
#cryptography       conda-forge/win-64::cryptography-50.0.1-py311h67a9792_1 
#earthengine-api    conda-forge/noarch::earthengine-api-1.7.43-pyhd8ed1ab_1 
#ee_extra           conda-forge/noarch::ee_extra-2025.7.2-pyhd8ed1ab_0 
#frozenlist         conda-forge/win-64::frozenlist-1.8.0-py313h0c48a3b_0 
#google-api-core    conda-forge/noarch::google-api-core-2.36.0-pyh5ded981_0 
#google-api-core-g~ conda-forge/noarch::google-api-core-grpc-2.36.0-pyh24e1d2c_0 
#google-api-python~ conda-forge/noarch::google-api-python-client-2.200.0-pyh5ded981_1 
#google-auth        conda-forge/noarch::google-auth-2.58.0-pyh5ded981_0 
#google-auth-httpl~ conda-forge/noarch::google-auth-httplib2-0.4.2-pyhcf101f3_0 
#google-cloud-core  conda-forge/noarch::google-cloud-core-2.7.0-pyhcf101f3_0 
#google-cloud-stor~ conda-forge/noarch::google-cloud-storage-3.14.1-pyh5ded981_0 
#google-crc32c      conda-forge/win-64::google-crc32c-1.8.0-py313h5327936_2 
#google-resumable-~ conda-forge/noarch::google-resumable-media-2.10.2-pyhcf101f3_0 
#googleapis-common~ conda-forge/noarch::googleapis-common-protos-1.75.3-pyh5ded981_0 
#googleapis-common~ conda-forge/noarch::googleapis-common-protos-grpc-1.75.3-pyh5ded981_0 
#grpc-google-iam-v1 conda-forge/noarch::grpc-google-iam-v1-0.14.5-pyhcf101f3_0 
#grpcio             conda-forge/win-64::grpcio-1.83.1-py313hc28e21e_1 
#grpcio-status      conda-forge/noarch::grpcio-status-1.83.1-pyhcf101f3_0 
#h2                 conda-forge/noarch::h2-4.4.1-pyhcf101f3_0 
#hpack              conda-forge/noarch::hpack-4.2.0-pyhd8ed1ab_0 
#httplib2           conda-forge/noarch::httplib2-0.32.0-pyhcf101f3_0 
#hyperframe         conda-forge/noarch::hyperframe-6.1.0-pyhd8ed1ab_0 
#icu                conda-forge/win-64::icu-78.3-h5112557_2 
#idna               conda-forge/noarch::idna-3.19-pyhcf101f3_0 
#importlib-metadata conda-forge/noarch::importlib-metadata-8.7.1-pyhcf101f3_0 
#libabseil          conda-forge/win-64::libabseil-20260526.0-cxx17_h04e5de1_2 
#libblas            conda-forge/win-64::libblas-3.11.0-11_h8455456_mkl 
#libcblas           conda-forge/win-64::libcblas-3.11.0-11_h2a3cdd5_mkl 
#libcrc32c          conda-forge/win-64::libcrc32c-1.1.2-h0e60522_0 
#libgrpc            conda-forge/win-64::libgrpc-1.83.1-he6cc664_1 
#libhwloc           conda-forge/win-64::libhwloc-2.13.0-default_h049141e_1000 
#libiconv           conda-forge/win-64::libiconv-1.18-hc1393d2_3 
#liblapack          conda-forge/win-64::liblapack-3.11.0-11_hf9ab0e9_mkl 
#libprotobuf        conda-forge/win-64::libprotobuf-7.35.1-h1a71995_3 
#libre2-11          conda-forge/win-64::libre2-11-2025.11.05-h45f70d9_2 
#libwinpthread      conda-forge/win-64::libwinpthread-12.0.0.r4.gg4f2fc60ca-h57928b3_10 
#libxml2            conda-forge/win-64::libxml2-2.15.4-he095d88_0 
#libxml2-16         conda-forge/win-64::libxml2-16-2.15.4-h3cfd58e_0 
#llvm-openmp        conda-forge/win-64::llvm-openmp-23.1.1-h49e36cd_0 
#mkl                conda-forge/win-64::mkl-2026.1.0-hac47afa_235 
#multidict          conda-forge/win-64::multidict-6.7.1-py313hd650c13_0 
#numpy              conda-forge/win-64::numpy-2.5.3-py313ha8dc839_0 
#opentelemetry-api  conda-forge/noarch::opentelemetry-api-1.44.0-pyhd8ed1ab_0 
#propcache          conda-forge/win-64::propcache-0.5.2-py313hd650c13_0 
#proto-plus         conda-forge/noarch::proto-plus-1.28.4-pyhcf101f3_0 
#protobuf           conda-forge/win-64::protobuf-7.35.1-py313h91c73f5_3 
#pyasn1             conda-forge/noarch::pyasn1-0.6.4-pyhcf101f3_0 
#pyasn1-modules     conda-forge/noarch::pyasn1-modules-0.4.2-pyhd8ed1ab_0 
#pycparser          conda-forge/noarch::pycparser-3.0-pyhcf101f3_0 
#pyparsing          conda-forge/noarch::pyparsing-3.3.2-pyhcf101f3_0 
#pysocks            conda-forge/noarch::pysocks-1.7.1-pyh09c184e_7 
#python-gil         conda-forge/noarch::python-gil-3.13.15-h4df99d1_103 
#pyu2f              conda-forge/noarch::pyu2f-0.1.5-pyhd8ed1ab_1 
#re2                conda-forge/win-64::re2-2025.11.05-haef9524_2 
#requests           conda-forge/noarch::requests-2.34.2-pyhcf101f3_0 
#rsa                conda-forge/noarch::rsa-4.9.1-pyhd8ed1ab_0 
#six                conda-forge/noarch::six-1.17.0-pyhe01879c_1 
#tbb                conda-forge/win-64::tbb-2023.1.0-hdcfe883_0 
#typing-extensions  conda-forge/noarch::typing-extensions-4.16.0-h69aa097_0 
#typing_extensions  conda-forge/noarch::typing_extensions-4.16.0-pyhcf101f3_0 
#uritemplate        conda-forge/noarch::uritemplate-4.2.0-pyhd8ed1ab_0 
#urllib3            conda-forge/noarch::urllib3-2.7.0-pyhd8ed1ab_0 
#vs2015_runtime     conda-forge/win-64::vs2015_runtime-14.51.36247-h633cb9f_41 
#win_inet_pton      conda-forge/noarch::win_inet_pton-1.1.0-pyh7428d3b_8 
#yarl               conda-forge/win-64::yarl-1.24.5-py313hd650c13_0 
#zipp               conda-forge/noarch::zipp-4.1.0-pyhcf101f3_0 
#zstd               conda-forge/win-64::zstd-1.5.7-h534d264_7 


#Downloading and Extracting Packages: ...working... done
#Preparing transaction: ...working... done
#Verifying transaction: ...working... done
#Executing transaction: ...working... done
#WARNING conda.conda_pypi.main:notify_externally_managed_future(156): 
#  Did you know? You can install many PyPI packages with conda
#using the conda-pypi beta. Get started:
#  https://docs.conda.io/projects/conda/en/stable/new-features.html


#Well done! rgee has been successfully installed on your system.
#You need restart R to see changes (Windows users must terminate R!). After that, we recommend
#running ee_check() to perform a full check of all non-R rgee dependencies.
#Do you want restart your R session? 
  
#1: yes
#2: no

#Modify Chunk OptionsRun All Chunks AboveRun Current Chunk


ee_check()

#◉  Python version
#✔ [Ok] C:/Users/lucas/AppData/Local/r-miniconda/envs/rgee/python.exe v3.13
#◉  Python packages:
#✔ [Ok] numpy
#✔ [Ok] earthengine-api◉  Python version
#✔ [Ok] C:/Users/lucas/AppData/Local/r-miniconda/envs/rgee/python.exe v3.13
#◉  Python packages:
#✔ [Ok] numpy
#✔ [Ok] earthengine-api◉  Python version
#✔ [Ok] C:/Users/lucas/AppData/Local/r-miniconda/envs/rgee/python.exe v3.13
#◉  Python packages:
#  ✔ [Ok] numpy
#✔ [Ok] earthengine-api

ee_clean_user_credentials()
ee_Initialize()

#── rgee 1.1.8 ─────────────────────────────────────────────── earthengine-api 1.6.11 ── 
#✔ user: not_defined 
#✔ Initializing Google Earth Engine

# rgee::ee_install_upgrade()
# Channels:
#   - conda-forge
# Platform: win-64
# Collecting package metadata (repodata.json): ...working... done
# Solving environment: ...working... done

## Package Plan ##

# environment location: C:\Users\lucas\AppData\Local\R-MINI~1\envs\rgee
# 
# added / updated specs:
#   - earthengine-api==1.6.11
# 
# 
# The following packages will be downloaded:
#   
#   package                    |            build
# ---------------------------|-----------------
#   earthengine-api-1.6.11     |     pyhd8ed1ab_0         311 KB  conda-forge
# ------------------------------------------------------------
#   Total:         311 KB
# 
# The following packages will be DOWNGRADED:
#   
#   earthengine-api                       1.7.43-pyhd8ed1ab_1 --> 1.6.11-pyhd8ed1ab_0 



#Downloading and Extracting Packages: ...working... done
#Preparing transaction: ...working... done
#Verifying transaction: ...working... done
#Executing transaction: ...working... done
#WARNING conda.conda_pypi.main:notify_externally_managed_future(156): 
#  Did you know? You can install many PyPI packages with conda
#using the conda-pypi beta. Get started:
#  https://docs.conda.io/projects/conda/en/stable/new-features.html


#Well done! the Earth Engine Python API was successfully upgraded (1.6.11).
#rgee needs restart R to see changes.
#Do you want to continues? 
  
#  1: yes
#2: no


ee_Initialize()
# ── rgee 1.1.8 ─────────────────────────────────────────────── earthengine-api 1.6.11 ── 
# ✔ user: not_defined 
# ✔ Initializing Google Earth Engine:Aviso en py_call_impl(callable, call_args$unnamed, call_args$named) :
#   ee.ee_exception.EEException: Google Earth Engine API has not been used in project 962798793737 before or it is disabled. Enable it by visiting https://console.developers.google.com/apis/api/earthengine.googleapis.com/overview?project=962798793737 then retry. If you enabled this API recently, wait a few minutes for the action to propagate to our systems and retry.
# Run `reticulate::py_last_error()` for details.
# ✔ Initializing Google Earth Engine:  DONE!
#   Error en value[[3L]](cond): 
#   It looks like your EE credential has expired. Try running ee_Authenticate() again or clean your credentials ee_clean_user_credentials().

# Dónde busca rgee las credenciales
rgee::ee_get_earthengine_path()

# Dónde las guardó realmente Python
 reticulate::py_run_string("import os; print(os.path.expanduser('~'))")
 
# Error en rgee::ee_get_earthengine_path(): 
#   rgee_sessioninfo.txt does not exist, run rgee::ee_Initialize() to fixed.

reticulate::py_run_string("import os; print(os.path.expanduser('~'))")
 
 list.files("C:/Users/lucas/.config/earthengine", recursive = TRUE, all.files = TRUE)
 
 usethis::edit_r_environ()
# [1] "credentials"

 Sys.getenv("HOME")
 #[1] "C:/Users/lucas"
 
 rgee::ee_clean_user_credentials()
 library(rgee)
 ee_Initialize( project = "rgeeproj26")
 ee_Initialize(email = "lucasaca79@gmail.com", project = "rgeeproj26")
 
 # ── rgee 1.1.8 ─────────────────────────────────────────────── earthengine-api 1.6.11 ── 
 # ✔ user: not_defined 
 # ✔ Initializing Google Earth Engine:Aviso en py_call_impl(callable, call_args$unnamed, call_args$named) :
 #   ee.ee_exception.EEException: Project rgeeproj26 is not registered to use Earth Engine. Visit https://console.cloud.google.com/earth-engine/configuration?project=rgeeproj26 to register your project. See https://developers.google.com/earth-engine/guides/access for more details.
 # Run `reticulate::py_last_error()` for details.
 # ✔ Initializing Google Earth Engine:  DONE!
 #   Error en value[[3L]](cond): 
 #   It looks like your EE credential has expired. Try running ee_Authenticate() again or clean your credentials ee_clean_user_credentials().
 
 library(reticulate)
 ee <- import("ee")
 ee$Authenticate()

 ee_Initialize(email = "lucasaca79@gmail.com", project = "rgeeproj26")
 
 ee_Initialize(email = "lucasaca79@gmail.com", project = "rgeeproj26", drive = FALSE, gcs = FALSE)
 
 # ── rgee 1.1.8 ─────────────────────────────────────────────── earthengine-api 1.6.11 ── 
 # ✔ user: not_defined 
 # ✔ Initializing Google Earth Engine:  DONE!
 #   Error en value[[3L]](cond): 
 #   It looks like your EE credential has expired. Try running ee_Authenticate() again or clean your credentials ee_clean_user_credentials().
 
 