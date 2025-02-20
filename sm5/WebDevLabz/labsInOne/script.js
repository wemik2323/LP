const button = document.getElementById("buttonSend");

button.addEventListener("click", function () {
    const gotEmail = document.getElementById('email').value;
    if (gotEmail == "") {alert("Please enter E-mail!");}
    else {
        const emailInput = document.querySelector("input[id='email']");
        const isValidEmail = emailInput.checkValidity();
        if (!isValidEmail) {alert("Email is not valid!");}
    }
});