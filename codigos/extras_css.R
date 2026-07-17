##Extras Css's
leaflet_legend_css="
    .leaflet-control-layers, .leaflet-control-legend, .info.legend {
      border: none !important;
      border-radius: 12px !important; 
      box-shadow: 0 4px 15px rgba(0,0,0,0.15) !important; 
      padding: 12px !important;
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif !important;
      background: rgba(255, 255, 255, 0.9) !important; 
      backdrop-filter: blur(5px); 
    }
    .legend i {
      border-radius: 50%; 
      width: 15px !important;
      height: 15px !important;
      margin-right: 10px !important;
    }
    .legend-title {
      font-weight: bold;
      font-size: 1.1em;
      margin-bottom: 8px;
      color: #2c3e50;
    }
  "
tour_button_css="
      #start_tour {
        display: flex;
        align-items: center;
        justify-content: flex-start;
        width: 100%;
        padding: 10px 15px;
        margin-top: 10px;
        border: none;
        background: transparent;
        color: #fff;
        font-size: 15px;
        font-weight: 500;
        cursor: pointer;
        border-radius: 4px;
        transition: all 0.3s ease;
      }
      #start_tour:hover {
        background-color: rgba(255, 255, 255, 0.15);
      }
      #start_tour:focus {
        outline: none;
        background-color: rgba(255, 255, 255, 0.2);
      }
      #start_tour i {
        margin-right: 10px;
        font-size: 16px;
      }
    "
introjs_tooltip_css="
      .introjs-tooltip {
        background: linear-gradient(135deg, #ffffff 0%, #f8fbff 100%);
        border-radius: 18px;
        box-shadow: 0 16px 40px rgba(15, 23, 42, 0.18);
        border: 1px solid rgba(0, 102, 87, 0.12);
        max-width: 390px;
        color: #1f2937;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      .introjs-tooltiptext {
        font-size: 14px;
        line-height: 1.6;
      }
      .introjs-tooltiptext b {
        color: #006657;
      }
      .introjs-tooltiptext .tour-card {
        padding: 2px 0;
      }
      .introjs-tooltiptext .tour-list {
        list-style: none;
        padding-left: 0;
        margin: 10px 0 0;
      }
      .introjs-tooltiptext .tour-list li {
        display: flex;
        align-items: flex-start;
        gap: 8px;
        margin-bottom: 8px;
      }
      .introjs-tooltiptext .tour-bullet {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 18px;
        height: 18px;
        border-radius: 50%;
        background: linear-gradient(135deg, #006657, #0f766e);
        color: #fff;
        font-size: 10px;
        flex-shrink: 0;
        margin-top: 3px;
      }
      .introjs-tooltiptext .tour-badge {
        display: inline-block;
        margin-top: 6px;
        padding: 3px 8px;
        border-radius: 999px;
        background: #ecfdf5;
        color: #166534;
        border: 1px solid #a7f3d0;
        font-size: 11px;
        font-weight: 600;
      }
      .introjs-button {
        border-radius: 999px;
        padding: 7px 14px;
        background: #006657;
        color: #fff;
        border: none;
        box-shadow: 0 4px 10px rgba(0, 102, 87, 0.2);
      }
      .introjs-button:hover {
        background: #005245;
        color: #fff;
      }
      .introjs-prevbutton {
        background: #f3f4f6 !important;
        color: #374151 !important;
      }
      .introjs-skipbutton {
        color: #6b7280;
      }
      .introjs-helperLayer {
        box-shadow: 0 0 0 5000px rgba(15, 23, 42, 0.35) !important;
      }
    "
sidebar_last_child_css="
      #sidebarItemExpanded > ul > :last-child {
        position: absolute;
        bottom: 0;
        width: 100%;
      }

    "
sliderInputTiempoCss=
"
              .slider-filter-container {
                background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
                border-radius: 8px;
                padding: 15px;
                margin-bottom: 20px;
                box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                display:flex;
                flex-direction:row;
                align-items:center;
                justify-content: space-between;
              }
              .slider-filter-label {
                font-weight: 600;
                color: #2c3e50;
                margin-bottom: 10px;
                font-size: 14px;
                display: flex;
                align-items: center;
              }
              .filter-icon {
                margin-right: 8px;
                font-size: 16px;
                color: #e74c3c;
              }
              .slider-filter-note {
                padding: 10px 12px;
                border-radius: 4px;
                font-size: 13px;
                color: SaddleBrown;
                font-weight: 500;
                display: flex;
                align-items: center;
                width:30%
              }
              .filter-note-icon {
                margin-right: 8px;
                font-size: 14px;
              }
              .irs--shiny .irs-single{
              background-color:#006657 !important;
              }
              .irs--shiny .irs-bar, .irs--shiny .irs-from, .irs--shiny .irs-to{
              background: linear-gradient(90deg, rgb(0, 102, 87), rgb(0, 110, 90)) !important;
              }
              .box.box-solid.box-primary > .box-header {
              background:#006657 !important;
              }
              .nav-tabs-custom>.nav-tabs>li.active{
              border-top-color:#006657 !important
              }
              
            "
funcionColorearBotonBorrar=function(string){
  if(string=='remove'){
    return("
                   let botonBorrar=document.getElementsByClassName('leaflet-draw-edit-remove')[0]
                   if(botonBorrar){
                     console.log(botonBorrar)
                     botonBorrar.classList.remove('colorRojo')
                   }
                   "
           )
  }
    else{
      return("
                   let botonBorrar=document.getElementsByClassName('leaflet-draw-edit-remove')[0]
                   if(botonBorrar){
                     console.log(botonBorrar)
                     botonBorrar.classList.add('colorRojo')
                   }
                   ")
    }
  
}