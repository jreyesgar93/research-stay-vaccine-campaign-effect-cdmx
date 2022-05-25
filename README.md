# research-stay-vaccine-campaign-effect-cdmx
## Files: 

- `mes` directory contains the code created for the research project with MES
- `report` directory contains the code created for the research stay report. 

##  Clone the repository, update pip and generate a pyenv and renv. 

- Clone the following repository:


`git clone https://github.com/jreyesgar93/research-stay-vaccine-campaign-effect-cdmx.git`

- Update pip:

`pip install --upgrade pip`

- Create ([a virtual environment](https://github.com/pyenv/pyenv)), and activate pyenv:

```
pyenv virtualenv covid-vaccine-effect
pyenv activate covid-vaccine-effect
```

- Make sure to install the following packages: 

```
sudo apt install unzip
sudo apt install curl
```

#### Install packages 
Install the following python packages: 

```
pip install -r requirements.txt
```
#### Renv
- Create ([a virtual environment using renv](https://rstudio.github.io/renv/reference/activate.html)), and activate:

#### Download Data  
Run the following command to download the data: 

```
./download_data.sh
```
