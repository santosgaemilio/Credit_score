
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
  });