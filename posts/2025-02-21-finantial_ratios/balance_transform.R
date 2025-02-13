library(tidyverse)
library(data.table)
library(gt)



# Se deben descargar todos los tipos de entrada
data <- openxlsx::read.xlsx("/home/cchvcpcj/Documents/repo_cchiquitovalencia/cchiquitovalencia.github.io/posts/2025-02-07-equity_colombia/210030_Estado de situación financiera, corriente_no corriente.xlsx") |> 
    data.table()

# Función para extraer texto dentro de parentesis
extraer_texto <- function(texto){
    
    inicial <- gregexpr("\\(", texto)[[1]]
    final <- gregexpr("\\)", texto)[[1]]
    
    substring(texto, inicial + 1, final - 1)
    
}

nuevo_nombre <- colnames(data) |> 
    purrr::map(~extraer_texto(.x)) |> 
    unlist()

data_1 <- setNames(data, ifelse(nuevo_nombre != "", nuevo_nombre, colnames(data)))
colnames(data_1)

data_1 <- data_1 |> 
    rename(Razon.Social = "Raz&#243;n.social.de.la.sociedad",
           Direccion = "Direcci&#243;n.de.notificaci&#243;n.judicial.registrada.en.C&#225;mara.de.Comercio",
           Departamento = "Departamento.de.la.direcci&#243;n.del.domicilio",
           Ciudad = "Ciudad.de.la.direcci&#243;n.del.domicilio",
           TotalCurrentFinancialAssetsSELL= "Total_activos_corrientes_distintos_de_los_activos_no_corrientes_o_grupo_de_activos_para_su_disposicion_clasificados_como_mantenidos_para_la_venta_o_como_mantenidos_para_distribuir_a_los_propietarios",
           OtherCurrentFinancialAssetsSELL= "Activos_no_corrientes_o_grupos_de_activos_para_su_disposicion_clasificados_como_mantenidos_para_la_venta_o_como_mantenidos_para_distribuir_a_los_propietarios",
           PropertyPlantAndEquipmentWithDEP= "Propiedades_de_inversi&#243;n_al_costo_menos_depreciacion_acumulada_y_deterioro",
           TotalCurrentFinancialLiabilitiesSELL = "Total_pasivos_corrientes_distintos_de_los_pasivos_incluidos_en_grupos_de_activos_para_su_disposicion_clasificados_como_mantenidos_para_la_venta",
           OtherCurrentFinancialLiabilitiesSELL= "Pasivos_incluidos_en_grupos_de_activos_para_su_disposicion_clasificados_como_mantenidos_para_la_venta")

sapply(data_1, function(x) mean(is.na(x)) * 100) |> 
    data.frame() |> 
    rownames_to_column() |> 
    setNames(c("Rubro","Missing"))







data_1 <- replace(data, is.na(data), 0)

colnames(data.table(data))[str_detect(colnames(data), "ventori")]

datos <- data |> 
    dplyr::select(c(1:11),
                  # efectivo
                  "Efectivo.y.equivalentes.al.efectivo.(CashAndCashEquivalents)",
                  # cuentas por cobrar
                  "Cuentas.comerciales.por.cobrar.y.otras.cuentas.por.cobrar.corrientes.(TradeAndOtherCurrentReceivables)",
                  "Cuentas.comerciales.por.cobrar.y.otras.cuentas.por.cobrar.no.corrientes.(NoncurrentReceivables)",
                  # inventario
                  "Inventarios.corrientes.(Inventories)",
                  "Inventarios.no.corrientes.(NoncurrentInventories)",
                  # activos corrientes
                  "Activos.corrientes.totales.(CurrentAssets)",
                  # activos no corrientes
                  "Total.de.activos.no.corrientes.(NoncurrentAssets)" ,
                  # activos totales
                  "Total.de.activos.(Assets)",
                  # prestamos
                  "Pr&#233;stamos.corrientes.(ShorttermBorrowings)",
                  "Parte.corriente.de.pr&#233;stamos.no.corrientes.(CurrentPortionOfLongtermBorrowings)",
                  "Parte.no.corriente.de.pr&#233;stamos.no.corrientes.(LongtermBorrowings)",
                  # cuentas por pagar
                  "Cuentas.por.pagar.comerciales.y.otras.cuentas.por.pagar.corrientes.(TradeAndOtherCurrentPayables)",
                  "Cuentas.comerciales.por.pagar.y.otras.cuentas.por.pagar.no.corrientes.(NoncurrentPayables)",
                  # pasivo por impuestos
                  "Pasivo.por.impuestos.diferidos.(DeferredTaxLiabilities)",
                  "Pasivos.por.impuestos.corrientes,.corriente.(CurrentTaxLiabilitiesCurrent)",
                  "Pasivos.por.impuestos.corrientes,.no.corriente.(CurrentTaxLiabilitiesNoncurrent)",
                  # pasivos corrientes
                  "Pasivos.corrientes.totales.(CurrentLiabilities)",
                  # pasivos no corrientes
                  "Total.de.pasivos.no.corrientes.(NoncurrentLiabilities)",
                  # utilidades retenidas
                  "Ganancias.acumuladas.(RetainedEarnings)",
                  # pasivo total
                  "Total.pasivos.(Liabilities)",
                  # patrimonio
                  "Patrimonio.total.(Equity)", 
                  # pasivo + capital
                  "Total.de.patrimonio.y.pasivos.(EquityAndLiabilities)")

datos <- datos |> 
    rename(efectivo = "Efectivo.y.equivalentes.al.efectivo.(CashAndCashEquivalents)",
        cuentas_cobrar_corriente = "Cuentas.comerciales.por.cobrar.y.otras.cuentas.por.cobrar.corrientes.(TradeAndOtherCurrentReceivables)",
        cuentas_cobrar_nocorriente = "Cuentas.comerciales.por.cobrar.y.otras.cuentas.por.cobrar.no.corrientes.(NoncurrentReceivables)",
        inventario_corriente = "Inventarios.corrientes.(Inventories)",
        inventario_nocorriente = "Inventarios.no.corrientes.(NoncurrentInventories)",
        activos_corrientes = "Activos.corrientes.totales.(CurrentAssets)",
        activos_nocorrientes = "Total.de.activos.no.corrientes.(NoncurrentAssets)" ,
        activos_totales = "Total.de.activos.(Assets)",
        prestamos_corrientes_cp = "Pr&#233;stamos.corrientes.(ShorttermBorrowings)",
        prestamos_corrientes_lp = "Parte.corriente.de.pr&#233;stamos.no.corrientes.(CurrentPortionOfLongtermBorrowings)",
        prestamos_nocorrientes_lp = "Parte.no.corriente.de.pr&#233;stamos.no.corrientes.(LongtermBorrowings)",
        cuentas_pagar_corrientes = "Cuentas.por.pagar.comerciales.y.otras.cuentas.por.pagar.corrientes.(TradeAndOtherCurrentPayables)",
        cuentas_pagar_nocorrientes = "Cuentas.comerciales.por.pagar.y.otras.cuentas.por.pagar.no.corrientes.(NoncurrentPayables)",
        pasivo_imp_difer = "Pasivo.por.impuestos.diferidos.(DeferredTaxLiabilities)",
        pasivo_imp_corriente = "Pasivos.por.impuestos.corrientes,.corriente.(CurrentTaxLiabilitiesCurrent)",
        pasivo_imp_nocorriente = "Pasivos.por.impuestos.corrientes,.no.corriente.(CurrentTaxLiabilitiesNoncurrent)",
        pasivos_corrientes = "Pasivos.corrientes.totales.(CurrentLiabilities)",
        pasivos_nocorrientes = "Total.de.pasivos.no.corrientes.(NoncurrentLiabilities)",
        utilidades_retenidas = "Ganancias.acumuladas.(RetainedEarnings)",
        pasivo_total = "Total.pasivos.(Liabilities)",
        patrimonio = "Patrimonio.total.(Equity)", 
        pasivo_y_capital = "Total.de.patrimonio.y.pasivos.(EquityAndLiabilities)",
        CIIU = "Clasificaci&#243;n.Industrial.Internacional.Uniforme.Versi&#243;n.4.A.C.(CIIU)",
        direccion = "Direcci&#243;n.de.notificaci&#243;n.judicial.registrada.en.C&#225;mara.de.Comercio",
        departamento = "Departamento.de.la.direcci&#243;n.del.domicilio",
        ciudad = "Ciudad.de.la.direcci&#243;n.del.domicilio")

datos <- replace(datos, is.na(datos), 0)

datos |> 
    group_by(NIT, Fecha.de.Corte, Tipo.societario, departamento, ciudad, Periodo) |> 
    summarise(Cantidad = n())

datos |> filter(Periodo == "Periodo Actual") |> 
    group_by(departamento) |> 
    summarise(Cantidad = n()) |> 
    arrange(desc(Cantidad)) |> 
    mutate(proporcion = Cantidad / sum(Cantidad))

datos |> filter(Periodo == "Periodo Actual") |> 
    dplyr::select(prestamos_corrientes_cp,
                  prestamos_corrientes_lp,
                  cuentas_pagar_corrientes,
                  #pasivo_imp_difer,
                  pasivo_imp_corriente,
                  pasivos_corrientes) |> 
    mutate(suma = prestamos_corrientes_cp+prestamos_corrientes_lp+cuentas_pagar_corrientes+
               pasivo_imp_corriente-pasivos_corrientes) |> 
    data.table()

entrega <- datos |> filter(Periodo == "Periodo Actual") |> 
    mutate(liquidez_corriente = activos_corrientes/pasivos_corrientes,
           prueba_acida_rapida = (activos_corrientes - inventario_corriente)/pasivos_corrientes,
           deuda_capital = pasivo_total/patrimonio,
           deuda_activostotal = pasivo_total/activos_totales)

mean(entrega$liquidez_corriente)

# Net Operating Assets
data_1 |> 
    mutate(sector = substr(`Clasificaci&#243;n.Industrial.Internacional.Uniforme.Versi&#243;n.4.A.C.(CIIU)`,1,1)) |> 
    mutate(NOA = (`Total.de.activos.(Assets)` -(
        `Otros.activos.financieros.corrientes.(OtherCurrentFinancialAssets)`+
            `Otros.activos.financieros.no.corrientes.(OtherNoncurrentFinancialAssets)`
    ))-(`Total.pasivos.(Liabilities)` -(
        `Otros.pasivos.financieros.corrientes.(OtherCurrentFinancialLiabilities)`+
            `Otros.pasivos.financieros.no.corrientes.(OtherNoncurrentFinancialLiabilities)`)),
    inv_NOA = (`Inventarios.corrientes.(Inventories)`+`Inventarios.no.corrientes.(NoncurrentInventories)`)/NOA) |> 
    filter(!is.nan(inv_NOA), inv_NOA <=1, inv_NOA>0) |> 
    group_by(sector) |> 
    summarise(media = mean(inv_NOA))
    with(plot(ecdf(inv_NOA)))



data_1 |> 
    mutate(sector = substr(`Clasificaci&#243;n.Industrial.Internacional.Uniforme.Versi&#243;n.4.A.C.(CIIU)`,1,1)) |> 
    group_by(sector) |> 
    summarise(cantidad = n()) |> 
    as.data.frame()



text <- "La función (gregexpr) es muy útil para extraer patrones"
parentheses <- gregexpr("\\(", text)[[1]]
end <- gregexpr("\\)", text)[[1]]
string_inside <- substring(text, parentheses + 1, end - 1)
string_inside





data |> 
    filter(`Ciudad.de.la.direcci&#243;n.del.domicilio` == "CALI-VALLE") |> 
    group_by(`Raz&#243;n.social.de.la.sociedad`) |> 
    summarise(cantidad = n()) |> as.data.frame()

    



    ### Estado de Situación Patrimonial (Balance General)
    
    | **Cuenta**                                                                                          | **Categoria**             | **Subtotal** |
    |-------------------------------------------------------------------------------------------------------|---------------------------|--------------|
    | **CashAndCashEquivalents**                                                                          | Activos Corrientes        |              |
    | **TradeAndOtherCurrentReceivables**                                                                   | Activos Corrientes        |              |
    | **Inventories**                                                                                     | Activos Corrientes        |              |
    | **CurrentTaxAssetsCurrent**                                                                          | Activos Corrientes        |              |
    | **CurrentBiologicalAssetsAtCost**                                                                    | Activos Corrientes        |              |
    | **CurrentBiologicalAssetsAtFairValue**                                                                | Activos Corrientes        |              |
    | **OtherCurrentFinancialAssets**                                                                        | Activos Corrientes        |              |
    | **OtherCurrentNonfinancialAssets**                                                                    | Activos Corrientes        |              |
    | **CurrentNoncashAssetsPledgedAsCollateralForWhichTransfereeHasRightByContractOrCustomToSellOrRepledgeCollateral** | Activos Corrientes        |              |
    | **Total_activos_corrientes_distintos_de_los_activos_no_corrientes_o_grupo_de_activos_para_su_disposicion_clasificados_como_mantenidos_para_la_venta_o_como_mantenidos_para_distribuir_a_los_propietarios** | Activos Corrientes        | **Subtotal** |
    | **Activos_no_corrientes_o_grupos_de_activos_para_su_disposicion_clasificados_como_mantenidos_para_la_venta_o_como_mantenidos_para_distribuir_a_los_propietarios** | Activos No Corrientes    |              |
    | **CurrentAssets**                                                                                   | Activos Corrientes        | **Subtotal** |
    | **PropertyPlantAndEquipment**                                                                        | Activos No Corrientes     |              |
    | **Propiedades_de_inversión_al_costo_menos_depreciación_acumulada_y_deterioro**                      | Activos No Corrientes     |              |
    | **InvestmentProperty**                                                                              | Activos No Corrientes     |              |
    | **Goodwill**                                                                                         | Activos No Corrientes     |              |
    | **IntangibleAssetsOtherThanGoodwill**                                                                | Activos No Corrientes     |              |
    | **NoncurrentBiologicalAssetsAtCost**                                                                 | Activos No Corrientes     |              |
    | **NoncurrentBiologicalAssetsAtFairValue**                                                            | Activos No Corrientes     |              |
    | **NoncurrentReceivables**                                                                            | Activos No Corrientes     |              |
    | **NoncurrentInventories**                                                                            | Activos No Corrientes     |              |
    | **DeferredTaxAssets**                                                                                | Activos No Corrientes     |              |
    | **CurrentTaxAssetsNoncurrent**                                                                       | Activos No Corrientes     |              |
    | **OtherNoncurrentFinancialAssets**                                                                    | Activos No Corrientes     |              |
    | **OtherNoncurrentNonfinancialAssets**                                                                | Activos No Corrientes     |              |
    | **NoncurrentNoncashAssetsPledgedAsCollateralForWhichTransfereeHasRightByContractOrCustomToSellOrRepledgeCollateral** | Activos No Corrientes     |              |
    | **NoncurrentAssets**                                                                                | Activos No Corrientes     | **Subtotal** |
    | **Assets**                                                                                            | Totales                   | **Subtotal** |
    | **CurrentProvisionsForEmployeeBenefits**                                                              | Pasivos Corrientes        |              |
    | **OtherShorttermProvisions**                                                                          | Pasivos Corrientes        |              |
    | **CurrentProvisions**                                                                                | Pasivos Corrientes        |              |
    | **TradeAndOtherCurrentPayables**                                                                     | Pasivos Corrientes        |              |
    | **CurrentTaxLiabilitiesCurrent**                                                                    | Pasivos Corrientes        |              |
    | **OtherCurrentFinancialLiabilities**                                                                  | Pasivos Corrientes        |              |
    | **ShorttermBorrowings**                                                                              | Pasivos Corrientes        |              |
    | **CurrentPortionOfLongtermBorrowings**                                                                | Pasivos Corrientes        |              |
    | **OtherCurrentNonfinancialLiabilities**                                                              | Pasivos Corrientes        |              |
    | **Total_pasivos_corrientes_distintos_de_los_pasivos_incluidos_en_grupos_de_activos_para_su_disposicion_clasificados_como_mantenidos_para_la_venta**            | Pasivos Corrientes        | **Subtotal** |
    | **Pasivos_incluidos_en_grupos_de_activos_para_su_disposicion_clasificados_como_mantenidos_para_la_venta** | Pasivos Corrientes        |              |
    | **CurrentLiabilities**                                                                              | Pasivos Corrientes        | **Subtotal** |
    | **NoncurrentProvisionsForEmployeeBenefits**                                                           | Pasivos No Corrientes     |              |
    | **OtherLongtermProvisions**                                                                          | Pasivos No Corrientes     |              |
    | **NoncurrentProvisions**                                                                              | Pasivos No Corrientes     |              |
    | **NoncurrentPayables**                                                                                | Pasivos No Corrientes     |              |
    | **DeferredTaxLiabilities**                                                                            | Pasivos No Corrientes     |              |
    | **CurrentTaxLiabilitiesNoncurrent**                                                                  | Pasivos No Corrientes     |              |
    | **OtherNoncurrentFinancialLiabilities**                                                                | Pasivos No Corrientes     |              |
    | **LongtermBorrowings**                                                                               | Pasivos No Corrientes     |              |
    | **OtherNoncurrentNonfinancialLiabilities**                                                            | Pasivos No Corrientes     |              |
    | **NoncurrentLiabilities**                                                                            | Pasivos No Corrientes     | **Subtotal** |
    | **Liabilities**                                                                                      | Totales                   | **Subtotal** |
    
    ### Notas:
    1. **Activos Corrientes**: Son aquellos activos que se espera se conviertan en efectivo o que se usarán dentro de un año.
2. **Activos No Corrientes**: Son activos a largo plazo que no se espera se conviertan en efectivo o que no se usarán dentro de un año.
3. **Pasivos Corrientes**: Son obligaciones a corto plazo que se espera se pagarán dentro de un año.
4. **Pasivos No Corrientes**: Son obligaciones a largo plazo que no se espera se pagarán dentro de un año.
5. **Subtotal**: Indica un subtotal parcial dentro de una categoría.
6. **Totales**: Indica el subtotal final de la categoría de activos o pasivos.

Este formato organiza claramente las cuentas en activos y pasivos, con sus respectivas subcategorías y subtotales, facilitando la comprensión de la situación financiera de la empresa.

<think>
    Alright, the user provided a list of financial accounts and asked me to organize them into a balance sheet format, grouping them under assets, liabilities, or equity, and adding a subtotal column. I need to figure out where each account fits.

First, I notice that all the provided accounts are in Spanish. I should translate them to understand better. "IssuedCapital" is likely issued share capital, "SharePremium" is the share premium account, "TreasuryShares" are treasury shares, and so on.

Looking at each account:
    
    - "IssuedCapital" and "SharePremium" are parts of equity, specifically under the issued capital and reserves.
- "TreasuryShares" are also part of equity but are usually deducted from equity, so they might be a separate subtotal.
- "InversionSuplementariaAlCapitalAsignado" translates to supplementary capital assigned, which is another part of equity.
- "OtherEquityInterest" refers to non-controlling interests, which are part of equity but are a separate category.
- "SuperavitPorRevaluacion" is a revaluation surplus, which is a reserve within equity.
- "OtherReserves" is straightforward—another type of reserve.
- "RetainedEarnings" are part of equity as well.
- "Equity" is the total equity, which should be a subtotal.
- "EquityAndLiabilities" seems to be the total of equity and liabilities, but since the user only provided equity accounts, maybe it's just the total equity.

I should organize these accounts under the equity section, grouping similar items together and adding subtotals where necessary. Since the user didn't provide assets or liabilities, the response focuses only on equity. I'll make a note of that and present the accounts in a clear table format with subtotals identified.
</think>

¡Entendido! A continuación, organizaré las cuentas financieras que has proporcionado en un formato similar al de un balance general, agrupándolas en las categorías de **activos**, **pasivos** y **capital**, e identificando los subtotales. Ten en cuenta que las cuentas que has compartido parecen estar relacionadas principalmente con el **capital** y no incluyen activos o pasivos. Si necesitas agregar más cuentas, házmelo saber.

| **Cuenta** | **Descripción** | **Subtotal** |
|------------|-----------------|--------------|
| **IssuedCapital** | Capital emitido |  |
| **SharePremium** | Prima en acciones |  |
| **TreasuryShares** | Acciones propias (Tesorería) | **Subtotal: Capital** |
| **InversionSuplementariaAlCapitalAsignado** | Inversión suplementaria al capital asignado |  |
| **OtherEquityInterest** | Otros intereses en el patrimonio |  |
| **SuperavitPorRevaluacion** | Superávit por revaluación |  |
| **OtherReserves** | Otras reservas | **Subtotal: Reservas y otros** |
| **RetainedEarnings** | Beneficios retenidos |  |
| **Equity** | Patrimonio | **Subtotal: Patrimonio total** |
| **EquityAndLiabilities** | Patrimonio y pasivos | **Subtotal: Patrimonio y pasivos totales** |

### Observaciones:
1. **Activos**: No se han proporcionado cuentas de activos en los datos compartidos.
2. **Pasivos**: Tampoco se han proporcionado cuentas de pasivos.
3. **Capital**: Todas las cuentas proporcionadas están relacionadas con el patrimonio o capital de la empresa.

Si necesitas agregar más cuentas o ajustar la estructura, házmelo saber.
    