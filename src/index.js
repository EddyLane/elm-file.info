import './main.css';
import {Elm} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

(function ({ports: {uploadSub, uploadCmd}}) {


    const msgs = {
        encode: 'encode',
        progress: 'progress',
        upload: 'upload',
        cancel: 'cancel',
        openFileBrowser: 'open-file-browser'
    };

    uploadCmd.subscribe(({message, uploadId, data}) => {
        switch (message) {

            case msgs.encode:
                handlers.encode({uploadId, data});
                break;

            case msgs.upload:
                handlers.upload({uploadId, data});
                break;

            case msgs.openFileBrowser:
                handlers.openFileBrowser(data);
                break;

            default:
                console.warn(`Unhandled message ${message}`);

        }
    });

    const handlers = {

        openFileBrowser: (inputId) => {
            console.info(`[elm-file] openFileBrowser (${inputId})`);

            const element = document.getElementById(inputId);

            if (element) {
                element.click();
            }
        },

        encode: ({uploadId, data}) => {

            console.debug(`[elm-file] Read base64 contents started (${uploadId})`);

            const reader = new FileReader();

            reader.onload = (({target: {result}}) => {

                console.info(`[elm-file] Read base64 contents success (${uploadId})`);

                uploadSub.send({
                    uploadId,
                    message: msgs.encode,
                    data: result
                });

            });

            reader.onerror = () => {

                console.error(`[elm-file] Read base64 contents failure (${uploadId})`);

                uploadSub.send({
                    uploadId,
                    message: msgs.encode,
                    data: {error: "Unable to read file"}
                });
            };

            reader.readAsDataURL(data);

        },

        upload: ({uploadId, data: {uploadUrl, base64Data, additionalData}}) => {

            console.debug(`[elm-file] Upload started to ${uploadUrl} (${uploadId})`);
            console.debug(`[elm-file] Upload additional data (${uploadId})`, additionalData);

            function upload(blob) {

                const uploadRequest = new XMLHttpRequest();

                const cancelHandler = (msg) => {

                    debugger;

                    if (msg.message !== msgs.cancel || uploadId !== msg.uploadId) {
                        return;
                    }

                    console.info(`[elm-file] Upload cancelled (${uploadId})`);
                    uploadRequest.abort();

                    uploadCmd.unsubscribe(cancelHandler);

                };

                uploadCmd.subscribe(cancelHandler);

                uploadRequest.open('POST', uploadUrl, true);

                uploadRequest.onload = () => {
                    if ([200, 201].indexOf(uploadRequest.status) === -1) {
                        return error();
                    }

                    return success();
                };

                uploadRequest.onerror = () => error();
                uploadRequest.upload.addEventListener('progress', (e) => progress(e));

                function progress(event) {
                    if (event.lengthComputable) {

                        const progress = event.loaded / event.total * 100;

                        console.debug(`[elm-file] Upload progress ${parseFloat(progress)} (${uploadId})`);

                        uploadSub.send({
                            uploadId,
                            message: msgs.progress,
                            data: progress
                        });

                    }
                }

                function success() {

                    console.info(`[elm-file] Upload success (${uploadId})`, uploadRequest.response);

                    const data = JSON.parse(uploadRequest.response);

                    uploadSub.send({
                        uploadId,
                        message: msgs.upload,
                        data
                    });

                    uploadCmd.unsubscribe(cancelHandler);
                }

                function error() {
                    console.error(`[elm-file] Upload failure (${uploadId})`);

                    uploadSub.send({
                        uploadId,
                        message: msgs.upload,
                        data: {
                            error: `${uploadRequest.status}; ${uploadRequest.statusText} ${uploadRequest.responseText}`
                        }
                    });

                    uploadCmd.unsubscribe(cancelHandler);
                }

                const formData = new FormData();
                formData.append('data', blob);
                formData.append('fileName', additionalData);

                uploadRequest.send(formData);

            }

            fetch(base64Data)
                .then(res => res.blob())
                .catch(() => {
                    console.error(`[elm-file] Upload failure (${uploadId})`);

                    uploadSub.send({
                        uploadId,
                        message: msgs.upload,
                        data: {error: "Failed to decode Base64 contents"}
                    });

                })
                .then(upload);
        }


    };


})(Elm.Main.init());


registerServiceWorker();