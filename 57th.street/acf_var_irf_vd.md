#visualizing the data
plot(MSCI) # 時系列プロット
summary(coredata(MSCI)) # 株式指数の記述統計量
acf(coredata(MSCI)) # 株式指数の自己相関


#visualizing the data with log return rate
JP.r <- dailyReturn(MSCI$JP, type = 'log') * 100 # 日本
UK.r <- dailyReturn(MSCI$UK, type = 'log') * 100 # イギリス
US.r <- dailyReturn(MSCI$US, type = 'log') * 100 # アメリカ
MSCI.r <- cbind(JP.r, UK.r, US.r) # データを結合
names(MSCI.r) <- names(MSCI) # 名を変更


plot(MSCI.r) # 時系列プロット
summary(coredata(MSCI.r)) # 株式指数の対数収益率の記述統計量
acf(coredata(MSCI.r)) # 株式指数の対数収益率の自己相関



#estimation
#decide the number of order with information criteria
library(vars) # パッケージの読込
(IC <- VARselect(MSCI.r)) # 情報量基準


#AICやBICの値が最小の次数を採用する。
#AICは、予測能力が最良のモデル（汎化誤差の最も小さなモデル）を良いと考えるのに対し、BICは真のモデルである確率は最も大きいモデルを良いと考える。
#各情報量基準に応じて結果が変化する場合、
#それぞれの次数でパラメータ推定を行ってみる。


m.a <- VAR(MSCI.r, lag.max = 10, ic = "AIC") # AICによるモデルの推定
m.b <- VAR(MSCI.r, lag.max = 10, ic = "SC") # BICによるモデルの推定

summary(m.b) # 推定結果を表示

#Confirm the validation of the model
#残差の自己相関を求めてモデルの妥当性を判断する
acf(residuals(m.a)) # 残差の自己相関
acf(residuals(m.b)) # 残差の自己相関 

#カバン検定を行う
serial.test(m.a, lags.pt = 10, type = "PT.adjusted") # ラグ10
serial.test(m.a, lags.pt = 15, type = "PT.adjusted") # ラグ15
serial.test(m.a, lags.pt = 20, type = "PT.adjusted") # ラグ20

#全てにおいてp値が0.05より大きいので, 有意水準5%で帰無仮説(自己相関がない）を棄却できない. つまり, 残差に自己相関があると判断できないので, 適切である．


#分析
#granger causalityの検証
causality(m.a, cause = "JP") # JP → UK, US
#これは, 1つの変数の他の全ての変数に対する検定であり, 他の1つの変数に対する検定ではない. 関数「causality」では, 他の1つの変数に対する検定を行うには2変量VARモデルを用いなければならない.
#2変量VARモデルに変換し、再度検証を行う。
JPUK <- cbind(MSCI.r$JP, MSCI.r$UK) # 日本とイギリスのデータ
m1.a <- VAR(JPUK, lag.max =10, ic = "AIC") # AICによるモデルの推定
m1.a$p # VARの次数
serial.test(m1.a, lags.pt = 15, type = "PT.adjusted") # ラグ15のかばん検定
causality(m1.a, cause = "JP") # JP → UK
#p値が0.05より大きいので, 有意水準5%で帰無仮説を棄却できない．つまり, 日本からイギリスへのグレンジャー因果性は存在すると判断できない．
causality(m1.a, cause = "UK") # UK → JP
#p値が0.05より小さいので, 有意水準5%で帰無仮説を棄却する．つまり, イギリスから日本へのグレンジャー因果性は存在すると判断できる．
JPUS <- cbind(MSCI.r$JP, MSCI.r$US) # 日本とアメリカのデータ
m2.a <- VAR(JPUS, lag.max =10, ic = "AIC") # AICによるモデルの推定
m2.a$p # VARの次数
serial.test(m2.a, lags.pt = 15, type = "PT.adjusted")# ラグ15のかばん検定
causality(m2.a, cause = "JP") # JP → US
#p値が0.05より大きいので, 有意水準5%で帰無仮説を棄却できない．つまり, 日本からアメリカへのグレンジャー因果性は存在すると判断できない．
causality(m2.a, cause = "US") # US → JP

UKUS <- cbind(MSCI.r$UK, MSCI.r$US) # イギリスとアメリカのデータ
m3.a <- VAR(UKUS, lag.max =10, ic = "AIC") # AICによるモデルの推定
m3.a$p # VARの次数
serial.test(m3.a, lags.pt = 15, type = "PT.adjusted") # ラグ15のかばん検定
causality(m3.a, cause = "UK") # UK → US
causality(m3.a, cause = "US") # US → UK

#inpulse response function
m.a.irf <- irf(m.a) # IRFを計算
plot(m.a.irf) # プロット

#variance decomposition
m.a.rvc <- fevd(m.a) # RVCを計算, Forecast Error Variance Decomposition
plot(m.a.rvc) # プロット
