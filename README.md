# Remesas, Índice de Tipo de Cambio Real y Competitividad: Un enfoque BVECM para Nicaragua

Este repositorio contiene el código, los datos y la documentación para reproducir los resultados del artículo de investigación que evalúa empíricamente la hipótesis de la Enfermedad Holandesa en Nicaragua frente al auge en la recepción de remesas durante el período 2006-2023.

## Resumen del Estudio

Para superar las limitaciones de sobreparametrización y capturar la dinámica de equilibrio de largo plazo ante quiebres estructurales, este estudio estima un Modelo de Vector de Corrección de Errores Bayesiano (BVECM). El análisis de las Funciones de Impulso-Respuesta (IRF) desestima la materialización de este síndrome macroeconómico en la economía nicaragüense.

## Hallazgos Principales

- No se evidencia un "efecto gasto" perjudicial, dado que las remesas no aprecian significativamente el Índice de Tipo de Cambio Real (ITCER).
- Este blindaje cambiario se atribuye a la alta propensión marginal a importar y al régimen cambiario administrado, los cuales actúan como válvulas de escape.
- Se rechaza la desindustrialización del sector transable.
- Los choques de divisas generan una expansión transitoria de la producción al aliviar las severas restricciones de liquidez y crédito local.
- Las remesas operan como un estabilizador macroeconómico, sin que se observe una pérdida significativa de competitividad cambiaria.

## Metodología y Datos

- **Muestra:** Datos trimestrales para la economía nicaragüense, abarcando el período 2006-2023.
- **Variables Endógenas:** Flujo de Remesas, Índice de Términos de Intercambio (ITI), Índice de Tipo de Cambio Real (ITCER), Índice de Producción Industrial de Estados Unidos (IPI EE. UU.) y el índice de actividad del Sector Transable (ST).
- **Software:** Todo el procesamiento y análisis econométrico se llevó a cabo en el entorno RStudio. La estimación bayesiana del VECM estructural se ejecutó con la librería `bsvars`; las pruebas de raíz unitaria, cointegración y diagnósticos se realizaron con los paquetes `urca`, `vars` y `coda`.

## Estructura del Repositorio

```         
├── dataset/ # Series trimestrales usadas en la estimación (remesas, ITCER, ITI, IPI EE. UU., ST).
│            # Ver README_Base_de_datos.md para el diccionario de variables y fuentes.
├── scripts/ # Scripts de R: limpieza de datos, pruebas de raíz unitaria/cointegración,
│           # estimación del BVECM (junto con sus otras vesiones -- BVAR --) y generación de las IRF.
├── graficos/ # Figuras generadas por los scripts (IRF, series de tiempo, diagnósticos).
├── paper_slices/ # Manuscrito del paper y diapositivas de la presentación.
├── README_Base_de_datos.md # Ignorar.
├── LICENSE # Licencia MIT — aplica al código en scripts/.
└── LICENSE-CONTENT.md # Licencia CC BY-NC-ND 4.0 — aplica al paper y las diapositivas en paper_slices/.
```

## Cómo Reproducir los Resultados

1.  **Clona el repositorio:**

    ``` bash
    git clone https://github.com/Emerson-nic/Bvar-Bvecm-Enfermedad_holandesa.git
    cd Bvar-Bvecm-Enfermedad_holandesa
    ```

2.  **Abre el proyecto en RStudio** y corre el script de instalación de dependencias. Las librerías necesarias (`bsvars`, `urca`, `vars`, `coda`, entre otras) se instalan y cargan automáticamente vía `pacman::p_load()`:

    ``` r
    options(repos = c(CRAN = "https://cloud.r-project.org"))
    if (!require("pacman")) install.packages("pacman")

    pacman::p_load(
    urca, tseries, dplyr, ggplot2, ...)
    ```

3.  Correr el script utilizado en el paper `bvecm.R`

4.  Los gráficos se guardan automáticamente en `graficos/`.

## Licencia

- El **código** (`scripts/`) se distribuye bajo licencia **MIT** — ver [`LICENSE`](./LICENSE). Libre para usar, modificar y redistribuir.
- El **paper** (solo el archivo de nexo `enfermedad_holadesa_nic.pdf`) se distribuyen bajo **Creative Commons Atribución-NoComercial-SinDerivadas 4.0 Internacional (CC BY-NC-ND 4.0)** — ver [`LICENSE-CONTENT.md`](./LICENSE-CONTENT.md), en línea con la política de acceso abierto de *Nexo Revista Científica*.

## Citar este repositorio / Artículo

Si utilizas el código o los datos de este repositorio en tu investigación, por favor cita el artículo original publicado en *Nexo Revista Científica*:

> Lopez, E. (2026). REMITTANCES, REAL EXCHANGE RATE AND COMPETITIVENESS: A BVECM APPROACH FOR NICARAGUA. *Nexo Revista Científica*, 39(01), 3-23. <https://doi.org/10.5377/nexo.v39i01.23169>

## Aclaraciones

Pido disculpas por las versiones anteriores de este repositorio: al inicio subía los archivos directo a GitHub sin ningún orden. Ya se reorganizó la estructura de carpetas y se agregó este README.

Los scripts publicados no son de la mejor calidad —código espagueti producto de mi poca experiencia— y pueden ser difíciles de leer. Gracias a este repositorio pude mejorar mi investigación como la forma en que comparto mis resultados, y prefiero que quede tal y como esta.

> **Nota sobre `README_Base_de_datos.md`:** ese archivo lo escribí cuando todavía tenía poca experiencia con GitHub, pensaba que la única forma de compartir un archivo `.xlsx` era de esa forma. Sobre notas de actualización de ese archivo las generó Copilot.

Gracias por leer, gracias Bonita, gracias Kuin, los quiero \<3

<table align="center">
  <tr>
    <td align="center" valign="middle"><img src="images/clipboard-674890819.png" width="84"/></td>
    <td align="center" valign="middle" style="padding: 0 90px;"><img src="https://raw.githubusercontent.com/twitter/twemoji/master/assets/72x72/2764.png" width="50"/></td>
    <td align="center" valign="middle"><img src="paper_slices/bonita.png" width="165"/></td>
  </tr>
  <tr>
    <td align="center"><b>KUIN</b></td>
    <td></td>
    <td align="center"><b>BONITA</b></td>
  </tr>
</table> 

## Contacto

Para dudas, sugerencias o colaboraciones relacionadas con este modelo econométrico, puedes abrir un *Issue* en este repositorio o contactar al autor principal a través de la publicación original.
