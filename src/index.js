import './main.css';
import {Main} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Main.embed(document.getElementById('root'));


app.ports.readFileContent.subscribe((request) => {

    const {id, data: {name, inputId}} = JSON.parse(request);
    const element = document.getElementById(inputId);

    if (!element && element.files) {
        console.error(`invalid inputId ${inputId}`);
        return;
    }

    [...element.files].forEach((file) => {

        if (file.name !== name) {
            return;
        }

        const reader = new FileReader();

        reader.onload = (({target: {result}}) => {
            app.ports.fileContentRead.send({id, result});
        });

        reader.readAsDataURL(file);
    });


});

app.ports.browseClick.subscribe((inputId) => {

    const element = document.getElementById(inputId);

    if (element) {
        element.click();
    }

});

registerServiceWorker();