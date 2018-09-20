import './main.css';
import {Main} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Main.embed(document.getElementById('root'));


app.ports.readFileContent.subscribe(([id, file]) => {

    const reader = new FileReader();

    reader.onload = (({target: {result}}) => {
        app.ports.fileContentRead.send({id, result})
    });

    reader.readAsDataURL(file);

});

app.ports.upload.subscribe(([id, signedUrl, base64Data]) => {

    fetch(base64Data)
        .then(res => res.blob())
        .then((blob) => {

            const uploadRequest = new XMLHttpRequest();
            uploadRequest.open('PUT', signedUrl, true);

            uploadRequest.onload = () => {
                app.ports.uploaded.send(id);
            };

            uploadRequest.send(blob);

        });

});

app.ports.browseClick.subscribe((inputId) => {

    const element = document.getElementById(inputId);

    if (element) {
        element.click();
    }

});

registerServiceWorker();