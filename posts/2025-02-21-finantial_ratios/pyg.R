library(tidyverse)
library(data.table)
library(gt)

pyg <- openxlsx::read.xlsx("/home/cchvcpcj/Documents/repo_cchiquitovalencia/cchiquitovalencia.github.io/posts/2025-02-07-equity_colombia/310030_Estado de resultado integral, resultado del periodo, por funcion de gasto(3).xlsx") |> 
    data.table()

colnames(pyg)

pyg |> 
    mutate(razon_cobertura = `Ganancia.(p&#233;rdida),.antes.de.impuestos.(ProfitLossBeforeTax)`/
               `Costos.financieros.(FinanceCosts)`)

levels(as.factor(pyg$Fecha.de.Corte))

table(pyg$Fecha.de.Corte)

pyg |> 
    filter(Periodo == "Periodo Actual")

pyg |> 
    filter(Periodo == "Periodo Actual") |> 
    filter(str_detect(`Raz&#243;n.social.de.la.sociedad`, "INTEGRADO"))


data |> 
    filter(Periodo == "Periodo Actual") |> 
    filter(str_detect(`Raz&#243;n.social.de.la.sociedad`, "INTEGRADO"))

colnames(data)

full_join(
data |> 
    filter(Periodo == "Periodo Actual") |> 
    dplyr::select(c(3,4,5,14,62,72)) |> 
    unique() 
,
pyg |> 
    filter(Periodo == "Periodo Actual") |> 
    dplyr::select(c(3,4,5,12)) |> 
    unique()
) |> 
    left_join(caratula[,c(3,11)], by = c("NIT")) |> 
    mutate(prop_deuda = `Total.pasivos.(Liabilities)` / `Total.de.patrimonio.y.pasivos.(EquityAndLiabilities)`,
           tiempo = 2024 - as.numeric(substr(`Fecha.de.constitución.(Aaaa-Mm-Dd)`,1,4))) |> 
    filter(prop_deuda > 0, !is.na(prop_deuda), prop_deuda<=1, tiempo >=0, !is.na(tiempo)) |> 
    filter(tiempo < 100) |> 
    ggplot(aes(as.numeric(tiempo), prop_deuda, group = as.factor(tiempo)))+
    geom_boxplot()


data |> 
    mutate(prop_deuda = `Total.pasivos.(Liabilities)` / `Total.de.patrimonio.y.pasivos.(EquityAndLiabilities)`) |> 
    filter(prop_deuda > 0, !is.na(prop_deuda), prop_deuda<=1) |> 
    with(hist(prop_deuda))


caratula <- openxlsx::read.xlsx("/home/cchvcpcj/Documents/repo_cchiquitovalencia/cchiquitovalencia.github.io/posts/2025-02-07-equity_colombia/10000_Carátula.xlsx") |> 
    data.table()

colnames(caratula)



