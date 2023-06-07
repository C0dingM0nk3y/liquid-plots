# liquid-plots
Tool for **visualization of LP positions from Binance liquid pools**. \
It also allow to **track earnings**, **calculate price levels for stop-loss/break-even**. \
Finally, it provide indications to **enter and exit pools at specific price ranges** in order to 
<span style="color:red;"> **take profit**</span> or 
<span style="color:red;">**accumulate desirable coins**</span> _(for long term investors that want to earn rewards AND accumulate their coin of interest)_.

***

Don't you use Binance pools yet? Uncertain about how to track Impermanent Losses? Open you first pool [here](https://www.binance.com/en/swap/pool)

New to Binance? You can use [this referral]( https://accounts.binance.com/register?ref=QBJI467Q) and enjoy a **5% fee rebate** (forever) while also contributing to this project support. \

***

## How to use? {.tabset}

### First time only

#### Install R-Studio and the required packages. 

To run this script it is required to install R-Studio from: https://posit.co/download/rstudio-desktop/

On the first run also install the following package

`install.packages("pacman") # package manager` 

This is the only package that requires being installed manually. \
Once the script is run for the first time, it will use `pacman` manager to download the other dependencies (if required).

***

#### Credentials
Login to your Binance account and reach the [API Management Page](https://www.binance.com/en/my/settings/api-management) 
 \
Click Create API. \
 \
Binance will provide you with a set of public and private keys. \ 

Open /credential.txt and replace the "dummy" keys with yours. Make sure to save as a ".txt" write the 2 keys on 2 lines, starting with the public one. \

`publicKey` <br>
`privateKey`

Save and replace original file. Alternatively, a different path to a file can be provided (keep the same format).

***

##### A note about safety.
Your **public key** is broadcasted on each API request. \
Your **private key** is stored locally, and never leaves your computer. It is used to sign API requests, and only the signature is broadcasted. So there is no risk for your private key to leave your computer. \
However, it is recommended to also **limit this key to a specific IP address** (this can be done from [Binance website](https://www.binance.com/en/my/settings/api-management)). This increases security, and it is of foremost importantance if it is desired to give this API key the rights to trade in your behalf (see note below)

<p style="margin-left: 40px"> Please note, in current version of the script, no transaction is ever performed in your behalf, and therefore it is sufficient give this API "read-only" rights. This will change if this program is used to automate transactions.</p>


### Run the script

#### Run the .Rmd notebook

1. Open the **liquid-plots.Rmd** file using R-Studio
2. From the toolbar menu, click on Knit, and select **Knit to HTML** (on the right of the Save Current Document icon)
3. Script will download all the required data and save it locally, then will perform the analysis and produce a .html file.
4. **Open the result file (html)** to visualize all the data.

<i>Note, in order for this script to have something to plot, it is required to have **at least one active pool on Binance** and to **have collected the pool rewards at least once**, I have also attached to this a zip file, that contains pre-downloaded data as a example.</i>

***

#### Options
First 2 chunks contain a few options that can be used to tune the script behavior.

Chunk 1: `general-options`

+ `keys.path`: can be used to provide a different path for credential path.

+ `run_api`: control the API behavior. If set to FALSE, no new data are downloaded, but the analysis is performed on local data.

+ `subset_list`: in case of users with many differnt pools, this can be used to select a subset of pools/coins to process. _uses REGEX syntax_ 

Chunk 2: `plot-options`

+ `setStopLoss` set stoploss

+ `takeProfit` set take profit (automatic exit of pool when most desirable coin is accumulated)


### Pre-made examples

#### 1. Pre-compiled "html"

Open the file _liq-pool_example.html_ to visualize an offline example with a few pools from my study sample.

***

#### 2. Run script on sample data

1. Unzip the content of _sample_data.zip_ and save the folder DOWNLOADS/ and TABLES/ into the root path (overwrite existing files, if any)

2. Navigate the .Rmd notebook to the first chunk: `general-options`. \ Select `run_api`= FALSE to disable API download.

3. Run the notebook file (click "knit" as usual)
