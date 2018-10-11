import './main.css';
import {Main} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Main.embed(document.getElementById('root'));


app.ports.readFileContent.subscribe(([id, file]) => {

    console.info(`PORT: Read base64 contents started (${id})`);

    const reader = new FileReader();

    reader.onload = (({target: {result}}) => {

        console.info(`PORT: Read base64 contents success (${id})`);

        app.ports.fileContentRead.send({id, result});
    });

    reader.onerror = () => app.ports.fileContentReadFailed.send(id);

    reader.readAsDataURL(file);

});


app.ports.uploadPort.subscribe(([id, signedUrl, base64Data, additionalData]) => {

    console.info(`PORT: Upload started to ${signedUrl} (${id})`);
    console.debug('Additional Data', additionalData);

    fetch(base64Data)
        .catch(() => {
            console.error(`PORT: Upload failure (${id})`);
            app.ports.uploadFailed.send(id);
        })
        .then(res => res.blob())
        .then((blob) => {

            const uploadRequest = new XMLHttpRequest();

            const cancelHandler = (cancelRequestId) => {
                if (id !== cancelRequestId) {
                    return;
                }
                console.info(`PORT: Upload cancelled (${id})`);
                uploadRequest.abort();
                app.ports.uploadCancelled.unsubscribe(cancelHandler);
            };

            app.ports.uploadCancelled.subscribe(cancelHandler);

            uploadRequest.open('POST', signedUrl, true);

            uploadRequest.onload = () => {
                const response = JSON.parse(uploadRequest.response);
                console.info(`PORT: Upload success (${id})`, response);
                app.ports.uploaded.send([id, response ]);
                app.ports.uploadCancelled.unsubscribe(cancelHandler);
            };

            uploadRequest.onerror = () => {
                console.error(`PORT: Upload failure (${id})`);
                app.ports.uploadFailed.send(id);
                app.ports.uploadCancelled.unsubscribe(cancelHandler);
            };

            uploadRequest.upload.addEventListener('progress', (event) => {

                if (event.lengthComputable) {

                    const progress = event.loaded / event.total * 100;

                    console.debug(`PORT: Upload progress ${parseFloat(progress)} (${id})`);

                    app.ports.uploadProgress.send([
                        id,
                        progress
                    ]);
                }
            });


            const formData = new FormData();
            formData.append('data', blob);
            formData.append('fileName', additionalData);

            uploadRequest.send(formData);

        });

});

app.ports.openFileBrowser.subscribe((inputId) => {

    console.info(`PORT: browseClick (${inputId})`);

    const element = document.getElementById(inputId);

    if (element) {
        element.click();
    }

});

registerServiceWorker();