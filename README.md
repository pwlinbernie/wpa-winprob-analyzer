# ⚾ WBC2013 Win Probability Analyzer  
**Reconstructing Win Probability & WPA for the 2013 WBC Taiwan vs Japan Game**

This project analyzes the 2013 WBC matchup between **Chinese Taipei (CT)** and **Japan (JP)** by reconstructing:

- 📈 **Win Probability (WP)** after every play  
- 🔥 **WPA (Win Probability Added)** for each batter  
- 📊 **Expected runs & scoring probabilities** based on base-out states  
- 🚨 **Key turning points** in the game  

The analysis follows the classic run-expectancy + win-probability framework found in Albert (2006) and modern sabermetrics.

---

## 📦 Project Structure
wpa-winprob-analyzer/
├─ data/
│ ├─ rpiunprob_albert.csv # Base-out scoring probabilities
│ ├─ winprob_by_inning_and_rundifferential.csv # Win prob table by inning × run diff
│ └─ WBC2013_CT_vs_JP.csv # Play-by-play data for the game
├─ WBC2013_winprob.R # Main analysis script
└─ README.md

---

## 🚀 Features

### ✅ 1. Compute Win Probability for Any Game State
Given:
- inning  
- half-inning (top/bottom)
- outs  
- baserunners  
- run differential  

the function `get_WP()` returns the **expected win probability** for the offense.

### ✅ 2. Reconstruct Full Game Win Probability Curve
The script processes play-by-play data and outputs:

- A win probability graph  
- Full WP timeline across all plays  
- WP momentum shifts inning by inning  

### ✅ 3. Compute WPA for Every Batter
WPA = change in WP caused by a player's action.

This allows you to identify:
- ⭐ Game MVP (highest WPA)
- ❌ Biggest negative impact (lowest WPA)
- 🔀 Turning points (biggest single-play WP change)

---

## 📊 Example Output (Graph / Table)

*待更新*

---

## 📥 How to Run the Script

### **1. Clone the repository**

```bash
git clone https://github.com/pwlinbernie/wpa-winprob-analyzer.git
cd wpa-winprob-analyzer
```

2. Open R or RStudio
Run:

source("WBC2013_winprob.R")

3. The script will:
Load all data files from /data/
Build run-expectancy tables (rp)
Load win-probability tables
Process the WBC2013 game
Output a WP plot + WPA summary

📚 Data Sources & References
1. run expectancy table (rpiunprob_albert.csv)

Based on:

Albert, J. (2006). Understanding Probability and Statistics in Baseball.

2. win probability table (winprob_by_inning_and_rundifferential.csv)

A pre-computed matrix mapping:

inning

run differential
to

win probability (home/visitor)

3. play-by-play data (WBC2013_CT_vs_JP.csv)

Manually collected PBP data for WBC2013 Taiwan vs Japan.

👤 Author

Po-Wei Lin（林柏緯）
Graduate Student @ National Tsing Hua University
Sports Analytics Enthusiast | R / Python | AI + Data Science

GitHub: https://github.com/pwlinbernie
