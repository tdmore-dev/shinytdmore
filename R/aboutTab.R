#'
#' Get the about tab panel.
#'
#' @return a panel
#'
getAboutTabPanel <- function() {
  panel <- tabPanel(
    "About",
    icon = icon("question"),
    HTML(
      "
      This is a demo application to show the possibilities of TDMore.<br/>
      The user interface can of course be adapted as required. We may even provide an API to transfer data. As an example:
      <p><b>Pull/push initiated by TDM app:</b> The TDM application runs
      as a client application. It polls Wintermute at specific timepoints,
      downloads the required data (patient information, patient covariates, dosing history and
      concentration samples), calculates the optimal next dose (and associated graphs and report),
      and automatically sends this back to Wintermute.</p>
      <p>Many alternatives for this 'upload results' exist:
      <ol>
      <li>Upload only the dose recommendation to Wintermute</li>
      <li>Upload only a warning to Wintermute 'New dosing recommendation available',
      with an associated ID. From the KWS, doctors can click the link and connect to
      a web app to further analyze the dosing recommendation.</li>
      <li>
      
      <p><b>Pull/push initiated by Wintermute:
      "
    )
    )
  return(panel)
}