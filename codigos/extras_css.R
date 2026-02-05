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