<?php
require_once "../config.php";
// start the session
session_start();
// Check if the user is not logged in, then redirect the user to login page
if ($_SESSION["userLoggedIn"] !== true) {
    header("location: userstatus/login.php");
}

//Read directory
if ($_SESSION['userStatus'] == 'donated' || $_SESSION['userStatus'] == 'admin') {
    
} else {
    
}

if ($_SESSION['userStatus'] == 'admin') {
    
}
?>
<!DOCTYPE html>
<html lang="en">
<title>Services</title>
<head>
    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
    <link href="https://fonts.googleapis.com/css2?family=Oxanium&family=Share+Tech+Mono&display=swap" rel="stylesheet">
    <link href="https://use.fontawesome.com/releases/v5.6.1/css/all.css" rel="stylesheet">
    <link href="../../styles/styles.css" rel="stylesheet">
    <link href="../../styles/fileMenu.css" rel="stylesheet">
    <link href="../../styles/tableStyle.css" rel="stylesheet">
</head>
<body>
    <div style="position:-webkit-sticky; 
                position:sticky; 
                top:0;
                color:#cf0101;
                background-color: #000000;
                background-image: url(../../styles/accbg.gif);
                height: 50px;
                width: 100%">
    </div>
    <div>
        <input type="checkbox" id="check">
        <label for="check">
            <i class="fas fa-bars" id="btn"></i>
            <i class="fas fa-times" id="cancel"></i>
        </label>
        <div class="sidebar">
            <header class="glitch" style="background-image: url(../../styles/accbg.gif);">Menu</header>
            <a href="../../welcome.php" class="active">
                <i class="fas fa-qrcode"></i>
                <span>Dashboard</span>
            </a>
            <a href="../account/yourAccount.php">
                <i class="fas fa-address-card"></i>
                <span>Account</span>
            </a>
            <a href="../overview/overview.php">
                <i class="fas fa-stream"></i>
                <span>Services</span>
            </a>

            <!-- IN PROGGRESS -->
            <a href="#">
                <i class="fas fa-calendar"></i>
                <span>Events</span>
            </a>
            <a href="#">
                <i class="far fa-question-circle"></i>
                <span>About</span>
            </a>
            <a href="#">
                <i class="fas fa-sliders-h"></i>
                <span>Setings</span>
            </a>
            <!-- IN PROGGRESS -->

            <a target="_blank" rel="noopener noreferrer" href="https://vk.com/wemik2323">
                <i class="far fa-envelope"></i>
                <span>Contact</span>
            </a>
            <a href="../../userstatus/logout.php">
                <i class="far fa-address-card"></i>
                <span>Log Out</span>
            </a>
        </div>
    </div>
</body>
    
<div class="Main_Window" style="margin-top:50px;">
    <h2 style="margin-bottom:30px;">Services</h2>
    <fieldset>
        <legend><p>Current services</p></legend>
        <table>
            <tr>
            <div class="card" style="margin: 15px">
                <div class="hover-text-one">
                    <figure class="effect-text-three">
                        <img src="../../styles/surg.jpg" alt=""/>
                        <figcaption>
                        <h3 style="font-family: Share Tech Mono">Surgeon</h3>
                        <p style="color: black">Description provided</p>
                        </figcaption>			
                    </figure>
                </div>
            </div>
            </tr>
            <tr>
            <div class="card" style="margin: 15px">
                <div class="hover-text-one">
                    <figure class="effect-text-three">
                        <img src="../../styles/ERrhXx9xxvk.jpg" alt=""/>
                        <figcaption>
                        <h3 style="font-family: Share Tech Mono">ENT doctor</h3>
                        <p style="color: black">Description provided</p>
                        </figcaption>			
                    </figure>
                </div>
            </div>
            </tr>
            <tr>
            <div class="card" style="margin: 15px">
                <div class="hover-text-one">
                    <figure class="effect-text-three">
                        <img src="../../styles/derm.jpg" alt=""/>
                        <figcaption>
                        <h3 style="font-family: Share Tech Mono">Dermatologist</h3>
                        <p style="color: black">Description provided</p>
                        </figcaption>			
                    </figure>
                </div>
            </div>
            </tr>
        </table>
            
    </fieldset>
</div>
<div style="height:100px; width:100%; margin-top:0; background-color:#000000; background-image:url(../../styles/accbg.gif); color:#ffffff;"></div>
</html>