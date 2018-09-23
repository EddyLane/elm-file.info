import './main.css';
import {Main} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Main.embed(document.getElementById('root'));


app.ports.readFileContent.subscribe(([id, file]) => {

    const reader = new FileReader();

    reader.onload = (({target: {result}}) => {
        app.ports.fileContentRead.send({id, result});
    });

    reader.readAsDataURL(file);

});

app.ports.upload.subscribe(([id, signedUrl, base64Data]) => {

    console.info(`Upload started (${id})`);

    fetch(base64Data)
        .catch(() => {


            console.error(`Upload failure (${id})`);
        })
        .then(res => res.blob())
        .then((blob) => {

            const uploadRequest = new XMLHttpRequest();

            const cancelHandler = (cancelRequestId) => {
                if (id !== cancelRequestId) {
                    return;
                }
                console.info(`Upload cancelled (${id})`);
                uploadRequest.abort();
                app.ports.uploadCancelled.unsubscribe(cancelHandler);
            };

            app.ports.uploadCancelled.subscribe(cancelHandler);

            uploadRequest.open('PUT', signedUrl, true);

            uploadRequest.onload = () => {
                console.info(`Upload success (${id})`);
                app.ports.uploaded.send(id);
                app.ports.uploadCancelled.unsubscribe(cancelHandler);
            };

            uploadRequest.onerror = () => {
                console.error(`Upload failure (${id})`);
                app.ports.uploadCancelled.unsubscribe(cancelHandler);
            };

            uploadRequest.upload.addEventListener('progress', (event) => {

                if (event.lengthComputable) {

                    const progress = event.loaded / event.total * 100;

                    console.debug(`Upload progress ${parseFloat(progress)} (${id})`);

                    app.ports.uploadProgress.send([
                        id,
                        progress
                    ]);
                }
            });

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