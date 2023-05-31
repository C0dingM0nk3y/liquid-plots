# liquid-plots
Tool for visualization of current LP positions from Binance account. It also allow to calculate price levels for stop-loss/break-even. 

## how to use? 

### first time only
Login to your Binance account and reach the API Management Page. [add link] 

Click Create API. 

Binance will provide you with a set of public and private keys. 

Open /credential.txt and replace the "dummy" keys with yours. Make sure to save as a ".txt" and separate name from key using ';' (prKey;Key). 
Save and replace original file. 

Note about safety. 
Your public key is transfered on each API request. 
Your private key is stored locally, used to sign API requests, and only the signature is broadcasted. So the Private key never leaves your computer.

_Please note, in current version of the script, all API calls are GET_
So it is recommended to disable all of the other functions from Binance API Management. This prevents this key pair to be able to make any edit (POST) in your behalf. 

Additionally, one may also limit the use of this key to a specific subset of IP address. See more [here. Add link] 

## Run analysis. 

To be written. 
