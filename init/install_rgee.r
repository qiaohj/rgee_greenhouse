library(remotes)
install_github("r-spatial/rgee")
library(rgee)
ee_clean_pyenv()

ee_install_set_pyenv(
  py_path = "/home/huijieqiao/.virtualenvs/rgee/bin/python", # Change it for your own Python PATH
  py_env = "rgee" # Change it for your own Python ENV
)
# apt install python3.10-venv
#install gcloud
#https://cloud.google.com/sdk/docs/install#deb
ee_install()
ee_check()
ee_Initialize(drive = T)
ee_install_upgrade()
