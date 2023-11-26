
document.addEventListener('DOMContentLoaded', function() {
    const logo = document.getElementById('logo');
    const logo_img = document.getElementById('logo-img');
    if (logo) {
      logo.addEventListener('click', function() {
        console.log('Logo clicked!');
      });
    }
    logo.addEventListener("mouseover", function() {
        logo_img.src = "Picture3.png";
      });
      
      logo.addEventListener("mouseout", function() {
        logo_img.src = "Picture2.png";
      });     
      
      let flag = true

    function changeText(){
      if(flag){
        solBtn.innerText = "changed"
        solucion.innerHTML = `<h1>Solución: </h1> <h2 id = "solucion-text">Crear un algoritmo de machine learning que sea útil 
           en la definición de puntajes de usuarios para la autorización de un crédito bancario.</h2>
           <p id = "solucion-subtext">Para esto analizaremos un dataset de un banco de la India, con 280 mil registros
           de potenciales aplicantes a préstamos.</p>
           <div id = "mas_ali"><img src="mas_btn.png" alt="" id="masBtn"></div>`
        title.style.height = "600px"
        body.style.display = "none"
        arrow.src = "arrow_up.png"
        const masBtn = document.getElementById("masBtn");  
        masBtn.addEventListener("mouseover", function() {
            masBtn.src = "mas_btn_sel.png";
        });
        masBtn.addEventListener("mouseout", function() {
          masBtn.src = "mas_btn.png";
        });
      }else{
        solBtn.innerText = "solucionar"
        solucion.innerHTML = ""
        title.style.height = "150px"
        body.style.display = ""
        arrow.src = "arrow.png"
      }    
      flag = !flag
    }
  
    const solBtn = document.getElementById("solucion-btn");
    const solucion = document.getElementById("solucion")
    const arrow = document.getElementById("arrow")
    const title = document.getElementById("main-page-title")
    const body = document.getElementById("main-page-body")
    arrow.addEventListener("click", changeText)  
    
    const sect1 =  document.getElementById("sect1")
    const sect1_cont = document.getElementById("sect1_text")
    sect1.addEventListener("click", function(){
      sect1_cont.style.display = ""
    })
  });

  
